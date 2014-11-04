#lang racket/base
(require racket/match
         racket/list
         racket/fixnum
         racket/flonum
         racket/runtime-path
         racket/math
         srpnt/music
         srpnt/music-theory
         srpnt/tracker
         srpnt/bithoven)
(module+ test
  (require rackunit))

(define-runtime-path clip-path "clip.raw.gz")
(define sample-bs (read-sample/gzip 0 4 clip-path))
;; XXX This is not a convenient interface, need a way to
;; fuse a DMC across the other frames
(cmd:hold*f 55
            (λ (f)
              (define s (fx* f samples-per-buffer))
              (cmd:frame* #f #f #f #f
                          (wave:dmc sample-bs s)
                          #f)))

;; A traditional ADSR is based on four phases of control:
;; - Attack: The level increases after the ON event for a time
;; - Delay: The level decrease after Attack is over
;; - Sustain: The level stays the same until OFF event
;; - Release: The level drops after the OFF event
;;
;; In my tracker, the amount of time an instrument is played is not
;; expressed as ON/OFF events, but we are called with an explicit time
;; (in frames). This means we don't really need to specify the sustain
;; parameter because we can compute it by subtracting the other
;; parameters.
;;
;; It would be natural to express these times as %s of the input frame
;; time, but in a real instrument when you hold a button on a piano
;; longer it doesn't make the attack longer, instead the attack is a
;; fixed amount of time, like 5ms.
;;
;; The problem then is that if I take explicit times, then if the
;; tempo is so fast that there would be no sustain phase, we want to
;; change it into a percentage. So, I will require the sustain
;; parameter to be specified to compute the expected total time.
;;
;; Then, rather than actual compute the parameter, we return whether
;; the process is in stage 0, 1, 2, or 3 and what percentage through
;; it you are. Further, we will allow the user to specify which phase
;; will fill extra space. [XXX Perhaps I should allow any combination
;; of expanders?]
(define (adsr expander attack decay sustain release)
  (define base (fx+ (fx+ attack decay) (fx+ sustain release)))
  (define base.0 (fx->fl base))
  (define-syntax-rule (define% attack% attack)
    (define attack% (fl/ (fx->fl attack) base.0)))
  (define% attack% attack)
  (define% decay% decay)
  (define% sustain% sustain)
  (define% release% release)
  (λ (total-frames)
    (define total-frames.0 (fx->fl total-frames))
    (define-syntax-rule (maybe-expand sustain attack decay release)
      (if (eq? expander 'sustain)
          (fx->fl (fx- total-frames (fx+ attack (fx+ decay release))))
          (fx->fl sustain)))
    (define-values (attack-len.0 decay-len.0 sustain-len.0 release-len.0)
      (if (fx< total-frames base)
          (values (fl* attack% total-frames.0)
                  (fl* decay% total-frames.0)
                  (fl* sustain% total-frames.0)
                  (fl* release% total-frames.0))
          (values (maybe-expand attack decay sustain release)
                  (maybe-expand decay attack sustain release)
                  (maybe-expand sustain attack decay release)
                  (maybe-expand release attack decay sustain))))
    (define attack-end.0 (fl+ 0.0 attack-len.0))
    (define decay-end.0 (fl+ attack-end.0 decay-len.0))
    (define sustain-end.0 (fl+ decay-end.0 sustain-len.0))
    (define release-end.0 (fl+ sustain-end.0 release-len.0))
    (λ (frame-i)
      (define frame-i.0 (fx->fl frame-i))
      (cond
       [(fl< frame-i.0 attack-end.0)
        (values 0 (fl/ frame-i.0 attack-len.0))]
       [(fl< frame-i.0 decay-end.0)
        (values 1 (fl/ (fl- frame-i.0 attack-end.0) decay-len.0))]
       [(fl< frame-i.0 sustain-end.0)
        (values 2 (fl/ (fl- frame-i.0 decay-end.0) sustain-len.0))]
       [else
        (values 3 (fl/ (fl- frame-i.0 sustain-end.0) release-len.0))]))))

;; A linear interpolaction is ideal for traditional ADSR which is
;; based on slopes

(define (linear lo hi %.0)
  (fx+ lo (fl->fx (flround (fl* %.0 (fx->fl (fx- hi lo)))))))

;; Vibrato, tremolo, as well as duty cycle modulation in a similar
;; framework are different oscillating effects

(define (modulate freq.0 base extent %.0)
  (define sx (flsin (fl* (fl/ freq.0 60.0) (fl* %.0 (fl* 2.0 pi)))))
  (define diff (fl->fx (flround (fl* (fx->fl extent) sx))))
  (fx+ base diff))

(module+ test
  (require plot)
  (plot-new-window? #t)
  (when #f
    (define (plot-one x-max)
      (define borders (vector '(0 . 10) '(10 . 5) '(5 . 5) '(5 . 0)))
      (define phase-f ((adsr 'sustain 5 5 10 5) x-max))
      (define (f x)
        (with-handlers ([exn:fail? (λ (x) (displayln (exn-message x)))])
          (define-values (which %) (phase-f (round x)))
          (match-define (cons lo hi) (vector-ref borders which))
          (define v (linear lo hi %))
          (if (= which 2)
              (modulate 440.0 v 2 %)
              v)))
      (function f #:color x-max #:label (format "frames = ~a" x-max)))
    (plot (list #;(plot-one 10)
           #;(plot-one 25)
           (plot-one 50))
          #:x-min 0 #:x-max 50
          #:y-min 0)))

(struct spec ())
(struct spec:constant spec (v))
(struct spec:adsr spec (expander a as d ds s ss r rs))
(struct spec:linear spec (lo hi))
(struct spec:apply spec (f ns))
(struct spec:modulate spec (freq base extent))
(struct staged ())
(struct staged:adsr spec (fun map))
(define (stage-spec s f)
  (match s
    [(spec:adsr e a as d ds s ss r rs)
     (staged:adsr ((adsr e a d s r) f) (vector as ds ss rs))]
    [_ s]))
(define (eval-spec s f)
  (match s
    [(staged:adsr adsr map)
     (define-values (which %) (adsr f))
     (eval-spec (vector-ref map which) %)]
    [(spec:constant v)
     v]
    [(spec:apply fun ns)
     (eval-spec ns (fun f))]
    [(spec:modulate freq base extent)
     (modulate freq base extent f)]
    [(spec:linear lo hi)
     (linear lo hi f)]))

(define (i:pulse/spec #:duty dspec #:period pspec #:volume vspec)
  (λ (frames tone*accent?)
    (define d* (stage-spec dspec frames))
    (define p* (stage-spec pspec frames))
    (define v* (stage-spec vspec frames))
    (match-define (cons tone accent?) tone*accent?)
    (define base-per (pulse-tone->period tone))
    (define base-volume (if accent? 1 0))
    (for/list ([f (in-range frames)])
      (define duty (fxmin 3 (fxmax 0 (eval-spec d* f))))
      (define per (fx+ base-per (eval-spec p* f)))
      (define volume (fxmin 15 (fxmax 0 (fx+ base-volume (eval-spec v* f)))))
      (wave:pulse duty per volume))))

(define (i:triangle/spec #:period pspec)
  (λ (frames tone*accent?)
    (define p* (stage-spec pspec frames))
    (match-define (cons tone accent?) tone*accent?)
    (define base-per (triangle-tone->period tone))
    (for/list ([f (in-range frames)])
      (define per (fx+ base-per (eval-spec p* f)))
      (wave:triangle #t per))))

(define (i:drum/spec #:mode mspec #:period pspec #:volume vspec)
  (λ (frames)
    (define m* (stage-spec mspec frames))
    (define p* (stage-spec pspec frames))
    (define v* (stage-spec vspec frames))
    (for/list ([f (in-range frames)])
      (define short? (eval-spec m* f))
      (define per (eval-spec p* f))
      (define volume (fxmin 15 (fxmax 0 (eval-spec v* f))))
      (wave:noise short? per volume))))

;; xxx look up better formulas?
(define i:drum:hihat
  (i:drum/spec #:mode (spec:constant #f)
               #:period (spec:constant #xC)
               #:volume
               (spec:adsr 'release
                          1 (spec:constant 4)
                          2 (spec:constant 3)
                          4 (spec:constant 2)
                          4 (spec:constant 0))))

(define i:drum:bass
  (i:drum/spec #:mode (spec:constant #f)
               #:period (spec:constant 9)
               #:volume
               (spec:adsr 'release
                          1 (spec:constant 10)
                          2 (spec:constant 7)
                          4 (spec:linear 4 2)
                          4 (spec:constant 0))))

(define i:drum:snare
  (i:drum/spec #:mode (spec:constant #f)
               #:period (spec:constant 7)
               #:volume
               (spec:adsr 'release
                          1 (spec:constant 11)
                          4 (spec:linear 11 6)
                          8 (spec:linear 6 2)
                          4 (spec:constant 0))))

(define (i:drums drums)
  (λ (frames which-n)
    ((vector-ref drums which-n) frames)))

;; 3, 4, 8 sound good
;; 9 is crunchy
;; 7 and C are okay

(define-syntax-rule (noise-line [len short? period vol] ...)
  (list* (cmd:hold* len (wave:noise short? period vol)) ...))

(define bass-drum
  (noise-line [1 #f 9 10] [2 #f 9 7] [3 #f 9 4] [3 #f 9 3] [4 #f 9 2]))
(define snare-drum1
  (noise-line [1 #f 7 11] [1 #f 7 9] [2 #f 7 8] [2 #f 7 7] [3 #f 7 4] [3 #f 7 3] [3 #f 7 2]))
(define snare-drum2
  (noise-line [1 #f 7 11] [1 #f 7 9] [1 #f 7 8] [1 #f 7 7] [1 #f 7 6] [2 #f 7 4] [2 #f 7 3] [4 #f 7 2]))
(define closed-hihat
  (noise-line [1 #f #xC 4] [2 #f #xC 3] [4 #f #xC 2] [4 #f #xC 1]))
(define loose-hihat
  (noise-line [1 #f #xC 7] [2 #f #xC 5] [4 #f #xC 4] [2 #f #xC 2] [4 #f #xC 2] [2 #f #xC 1]))
(define open-hihat
  (noise-line [1 #f #xC 6] [1 #f #xC 5] [3 #f #xC 4] [2 #f #xC 3] [4 #f #xC 2] [4 #f #xC 1]))

(define (i:pulse duty volume)
  (λ (frames tone*accent?)
    (match-define (cons tone accent?) tone*accent?)
    (define evolume (if accent? (fxmin 15 (fx+ 1 volume)) volume))
    (cmd:hold* frames
               (wave:pulse duty (pulse-tone->period tone) evolume))))

(define (i:triangle)
  (λ (frames tone*accent?)
    (match-define (cons tone accent?) tone*accent?)
    (cmd:hold* frames
               (wave:triangle #t (triangle-tone->period tone)))))

(define (i:pulse-plucky pluck% duty volume)
  (λ (frames tone*accent?)
    (match-define (cons tone accent?) tone*accent?)
    (define evolume (if accent? (fxmin 15 (fx+ 1 volume)) volume))
    (define per (pulse-tone->period tone))
    (define pluck-frames (fl->fx (flceiling (fl* (fx->fl frames) pluck%))))
    (define unpluck-frames (fx- frames pluck-frames))
    (define unpluck-frames-third (fxquotient unpluck-frames 3))
    (define unpluck-frames-final-third
      (fx- unpluck-frames (fx* unpluck-frames-third 2)))
    (list*
     (cmd:hold* pluck-frames
                (wave:pulse duty per evolume))
     (cmd:hold* unpluck-frames-third
                (wave:pulse (fxmax 0 (fx- duty 1)) per evolume))
     (cmd:hold* unpluck-frames-third
                (wave:pulse (fxmax 0 (fx- duty 1)) per (fxquotient evolume 2)))
     (cmd:hold* unpluck-frames-final-third
                (wave:pulse (fxmax 0 (fx- duty 1)) per (fxquotient evolume 4))))))

(define (i:pulse-slow-mod how-many duty volume)
  (λ (frames tone*accent?)
    (match-define (cons tone accent?) tone*accent?)
    (define evolume (if accent? (fxmin 15 (fx+ 1 volume)) volume))
    (define per (pulse-tone->period tone))
    (define part-frames (fl->fx (flceiling (fl/ (fx->fl frames) (fx->fl how-many)))))
    (define-values (_ l)
      (for/fold ([remaining frames] [l empty])
                ([n (in-range how-many)])
        (values
         (fx- remaining part-frames)
         (cons l
               (cmd:hold* (fxmin remaining part-frames)
                          (wave:pulse (fxmin 4 (fxmax 0 (if (even? n) duty (fx- duty 1))))
                                      per evolume))))))
    l))

(define (i:drum which-v)
  (λ (frames which-n)
    (define which (vector-ref which-v which-n))
    (cmd:ensure frames which)))

(define 237:Do-What-Is-Right
  (let ()
    (define s
      (scale-diatonic-major 'G))
    (define base-octave 4)
    (chorded-song->commands
     #:me (cons 0.25 116)
     #:ts ts:3:4
     #:drum (i:drum (vector closed-hihat
                            bass-drum
                            snare-drum2))
     #:drum-measure
     (list (cons 0.125 2)
           (cons 0.125 0)
           (cons 0.125 0)
           (cons 0.125 0)
           (cons 0.125 0)
           (cons 0.125 0))
     #:instruments
     (vector (cons (i:pulse-plucky 0.25 2 8) (fx+ base-octave 1))
             (cons (i:pulse-slow-mod 16 2 4) base-octave)
             (cons (i:triangle) (fx- base-octave 1)))
     #:measures
     (let ()
       (define phrase1
         (list
          ;; 1
          (list
           (list* 0.375 '((B . 0) (D . 0) (G . 0)) #f)
           (list* 0.125 '((A . 0) (C . 0) (G . 0)) #f)
           (list* 0.250 '((G . 0) (B . 0) (G . 0)) #f))
          ;; 2
          (list
           (list* 0.500 '((B . 0) (D . 0) (G . 0)) #f)
           (list* 0.250 '((G . 0) (B . 0) (G . 0)) #f))
          ;; 3
          (list
           (list* 0.250 '((A . 0) (C . 0) (D . 0)) #f)
           (list* 0.250 '((G . 0) (B . 0) (D . 0)) #f)
           (list* 0.250 '((A . 0) (C . 0) (D . 0)) #f))
          ;; 4
          (list
           (list* 0.250 '((B . 0) (D . 0) (G . 0)) #f)
           (list* 0.500 '((G . 0) (B . 0) (G . 0)) #f))
          ;; 5
          (list
           (list* 0.375 '((G . 0) (E . 0) (C . 0)) #f)
           (list* 0.125 '((F# . 0) (D . 0) (C . 0)) #f)
           (list* 0.250 '((E . 0) (C . 0) (C . 0)) #f))
          ;; 6
          (list
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((G . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((B . 0) (D . 0) (G . 0)) #f))
          ;; 7
          (list
           (list* 0.375 '((A . 0) (C . 0) (D . 0)) #f)
           (list* 0.125 '((G . 0) (B . 0) (D . 0)) #f)
           (list* 0.250 '((A . 0) (C . 0) (D . 0)) #f))
          ;; 8
          (list
           (list* 0.750 '((G . 0) (B . 0) (G . 0)) #f))))
       (define phrase2
         (list
          ;; 1
          (list
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f))
          ;; 2
          (list
           (list* 0.250 '((E . 0) (C . 0) (G . 0)) #f)
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f))
          ;; 3
          (list
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f))
          ;; 4
          (list
           (list* 0.250 '((E . 0) (C . 0) (G . 0)) #f)
           (list* 0.500 '((D . 0) (B . 0) (G . 0)) #f))
          ;; 5
          (list
           (list* 0.250 '((D . 0) (B . 0) (G . 1)) #f)
           (list* 0.250 '((G . 0) (B . 0) (G . 1)) #f)
           (list* 0.250 '((B . 0) (D . 0) (G . 1)) #f))
          ;; 6
          (list
           (list* 0.250 '((D . 1) (D . 0) (G . 1)) #f)
           (list* 0.250 '((B . 0) (D . 0) (G . 1)) #f)
           (list* 0.250 '((G . 0) (D . 0) (G . 0)) #f))
          ;; 7
          (list
           (list* 0.125 '((F# . 0) (D . 0) (A . 0)) #f)
           (list* 0.375 '((A . 0) (C# . 0) (A . 0)) #f)
           (list* 0.250 '((C# . 0) (A . 0) (A . 0)) #f))
          ;; 8
          (list
           (list* 0.750 '((D . 0) (A . 0) (D . 0)) #f))))
       (append phrase1
               phrase1
               phrase2
               phrase1)))))

(define (force-lazy-scale/tones scale rest? tones)
  (for/list ([t*o (in-list tones)]
             [can-be-rest? (in-list (list #t #t #f))])
    (if (and can-be-rest? rest?)
        #f
        (match t*o
          [#f
           #f]
          [(cons off t-oct)
           (match-define (cons tone s-oct) (list-ref/modify modify/octave scale off))
           (cons tone (fx+ t-oct s-oct))]))))
(define (force-lazy-scale/measures scale rest-n ms)
  (for/list ([ns (in-list ms)])
    (for/list ([n (in-list ns)])
      (match-define (list* note tones more) n)
      (define rest? (and rest-n (zero? (random rest-n))))
      (list* note (force-lazy-scale/tones scale rest? tones) more))))

;; xxx get more from here: http://en.wikipedia.org/wiki/Drum_beat
(define beat:heavy-metal
  (list (cons 0.125 1) (cons 0.0625 1) (cons 0.0625 1)
        (cons 0.125 2) (cons 0.0625 1) (cons 0.0625 1)
        (cons 0.125 1) (cons 0.0625 1) (cons 0.0625 1)
        (cons 0.125 2) (cons 0.0625 1) (cons 0.0625 1)))
(define beat:blast-beat
  (list (cons 0.125 1) (cons 0.125 2)
        (cons 0.125 1) (cons 0.125 2)
        (cons 0.125 1) (cons 0.125 2)
        (cons 0.125 1) (cons 0.125 2)))
(define beat:funk-beat
  (list (cons 0.125 1) (cons 0.125 0)
        (cons 0.125 2) (cons 0.125 0)
        (cons 0.125 1) (cons 0.125 1)
        (cons 0.125 0) (cons 0.125 2)))
(define beat:double-time
  (list (cons 0.0625 1) (cons 0.0625 0) (cons 0.0625 2) (cons 0.0625 0)
        (cons 0.0625 1) (cons 0.0625 0) (cons 0.0625 2) (cons 0.0625 0)
        (cons 0.0625 1) (cons 0.0625 0) (cons 0.0625 2) (cons 0.0625 0)
        (cons 0.0625 1) (cons 0.0625 0) (cons 0.0625 2) (cons 0.0625 0)))
(define beat:straight-rock
  (list (cons 0.125 1) (cons 0.125 0)
        (cons 0.125 2) (cons 0.125 0)
        (cons 0.125 1) (cons 0.125 0)
        (cons 0.125 2) (cons 0.125 0)))
(define beat:alternating-on
  (list (cons 0.125 0) (cons 0.125 0)
        (cons 0.125 1) (cons 0.125 0)
        (cons 0.125 0) (cons 0.125 0)
        (cons 0.125 2) (cons 0.125 0)))
(define beat:duple-triplets
  (list (cons 0.125 1) (cons 0.0625 0) (cons 0.0625 0)
        (cons 0.125 2) (cons 0.0625 0) (cons 0.0625 0)
        (cons 0.125 1) (cons 0.0625 0) (cons 0.0625 0)
        (cons 0.125 2) (cons 0.0625 0) (cons 0.0625 0)))

(define (select-drum-measure ts ap)
  ;; xxx make this more robust
  (cond
   [(eq? ts ts:4:4)
    (match ap
      ['(#t #f #f #f)
       (select-from-list
        (list
         (list (cons 0.125 1) (cons 0.125 0)
               (cons 0.125 0) (cons 0.125 0)
               (cons 0.125 0) (cons 0.125 0)
               (cons 0.125 0) (cons 0.125 0))
         (list (cons 0.125 2) (cons 0.125 0)
               (cons 0.125 0) (cons 0.125 0)
               (cons 0.125 0) (cons 0.125 0)
               (cons 0.125 0) (cons 0.125 0))))]
      [_
       (select-from-list
        (list
         beat:alternating-on
         beat:straight-rock
         beat:duple-triplets
         beat:double-time
         beat:blast-beat
         beat:funk-beat
         beat:heavy-metal))])]
   [(eq? ts ts:3:4)
    (list (cons 0.125 1) (cons 0.125 0)
          (cons 0.125 0) (cons 0.125 0)
          (cons 0.125 0) (cons 0.125 0))]))

;; xxx new interface: submit composition, arrangement, and
;; effects. have the player store some state (like what part, what
;; measure, what frame it is on) that gets updated every frame. when
;; you change arrangement, you may change tempo, so you port the
;; state. every frame, evaluate the composition on the frame number to
;; get the instruments.

(define (composition->track c)
  (define (convert scale-kind rest-n tempo)
    (define scale-root (select-from-list tone-names))
    (define scale (scale-kind scale-root))
    (convert-with scale rest-n tempo))

  (let ()
    (local-require racket/pretty)
    (pretty-print c))

  (match-define (vector ts ap pattern parts) c)
  ;; xxx choose this (and make sure every instrument can play the notes)
  (define base-octave 3)

  (define (convert-with scale rest-n tempo)
    (printf "Tempo is ~v\n" tempo)
    (chorded-song->commands*
     #:me
     (cons 0.25 tempo)
     #:ts ts
     #:drum
     ;; xxx generate this
     (cond
      [#t
       (i:drums (vector i:drum:hihat
                        i:drum:bass
                        i:drum:snare))]
      [(< 80 tempo 170)
       (i:drum (vector (select-from-list (list closed-hihat
                                               open-hihat
                                               loose-hihat))
                       bass-drum
                       (select-from-list (list snare-drum1
                                               snare-drum2))))]
      [else
       (i:drum (vector #f #f #f))])
     #:instruments
     ;; xxx generate this
     (let ()
       (match-define
        (list melody harmony bass)
        (cond
         [#t
          ((if #t (λ (x) x) shuffle)
           (list (i:pulse/spec
                  #:duty
                  (or (spec:constant 2)
                      (spec:adsr 'sustain
                                 0 (spec:constant 0)
                                 0 (spec:constant 0)
                                 1 (spec:modulate 440.0 2 1)
                                 0 (spec:constant 0)))
                  #:period
                  (or (spec:constant 0)
                      (spec:adsr 'sustain
                                 0 (spec:constant 0)
                                 0 (spec:constant 0)
                                 1 (spec:modulate 440.0 0 10)
                                 0 (spec:constant 0))
                      (spec:adsr 'sustain
                                 20 (spec:linear -5 5)
                                 05 (spec:linear 5 0)
                                 10 (spec:constant 0)
                                 05 (spec:linear 0 -5)))
                  #:volume
                  (or (spec:adsr 'sustain
                                 10 (spec:linear 10 10)
                                 10 (spec:linear 8 8)
                                 10 (spec:linear 6 6)
                                 10 (spec:linear 0 0))
                      (spec:constant 6)
                      (spec:adsr 'sustain
                                 05 (spec:linear 0 15)
                                 05 (spec:linear 15 6)
                                 10 (spec:modulate 440.0 6 4)
                                 05 (spec:linear 6 0))
                      (spec:adsr 'sustain
                                 05 (spec:linear 0 15)
                                 05 (spec:linear 15 6)
                                 10 (spec:linear 6 6)
                                 10 (spec:linear 6 0))))
                 (or
                  (i:pulse/spec
                   #:duty
                   (or (spec:constant 2)
                       (spec:adsr 'sustain
                                  0 (spec:constant 0)
                                  0 (spec:constant 0)
                                  1 (spec:modulate 880.0 2 1)
                                  0 (spec:constant 0)))
                   #:period (spec:constant 0)
                   #:volume (spec:constant 6))
                  (i:pulse/spec
                   #:duty (spec:constant 2)
                   #:period
                   (spec:adsr 'sustain
                              0 (spec:constant #f)
                              0 (spec:constant #f)
                              1 (spec:modulate 440.0 0 2)
                              0 (spec:constant #f))
                   #:volume (spec:adsr 'sustain
                                       4 (spec:constant 15)
                                       9 (spec:linear 15 6)
                                       0 (spec:constant 0)
                                       0 (spec:constant 0))))
                 (i:triangle/spec
                  #:period
                  (or (spec:adsr 'sustain
                                 0 (spec:constant 0)
                                 0 (spec:constant 0)
                                 1 (spec:modulate 880.0 0 5)
                                 0 (spec:constant 0))
                      (spec:constant 0)))))]
         [else
          (shuffle
           (list (if (zero? (random 2))
                     (i:pulse (+ 1 (random 2)) 6)
                     (i:pulse-plucky 0.25 (+ 1 (random 2)) 6))
                 (if (zero? (random 2))
                     (i:pulse (+ 1 (random 2)) 6)
                     (i:pulse-slow-mod 16 (+ 1 (random 2)) 6))
                 (i:triangle)))]))
       (vector (cons melody (fx+ base-octave 1))
               (cons harmony base-octave)
               (cons bass (fx- base-octave 1))))
     #:measures
     (append*
      (for/list ([p (in-list pattern)])
        (force-lazy-scale/measures scale rest-n (hash-ref parts p))))
     #:drum-measures
     (let ()
       (define part->dms (make-hash))
       (append*
        (for/list ([p (in-list pattern)])
          (hash-ref! part->dms p
                     (λ ()
                       (define dm (select-drum-measure ts ap))
                       (define ms (hash-ref parts p))
                       (for/list ([m (in-list ms)]) dm))))))))

  (when #f
    (list
     (convert scale-diatonic-major 8 480)
     (convert scale-diatonic-major 8 280)
     (convert scale-diminished #f 90)
     (convert scale-harmonic-minor 16 210)
     (convert (select-from-list scales)
              (and (zero? (random 2)) (+ 4 (random 8)))
              (+ 50 (random 400)))))

  (convert scale-diatonic-major 8 210))

(module+ main-x
  (play-one! (cmd:repeat (composition->track (bithoven)))))

(define main-track
  (composition->track (bithoven))
  #;
  (cmd:repeat
   (composition->track (bithoven))
   #;237:Do-What-Is-Right
   #;libbys-song))

(provide main-track)
