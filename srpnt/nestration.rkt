#lang racket/base
(require racket/match
         racket/list
         racket/fixnum
         racket/flonum
         racket/math
         srpnt/music
         srpnt/music-theory
         srpnt/tracker)

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

(define (select-from-list l)
  (list-ref l (random (length l))))
(provide select-from-list)

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

(define (nestration)
  #f)

;; xxx new interface: submit composition, arrangement, and
;; effects. have the player store some state (like what part, what
;; measure, what frame it is on) that gets updated every frame. when
;; you change arrangement, you may change tempo, so you port the
;; state. every frame, evaluate the composition on the frame number to
;; get the instruments.

(define (composition->track c a)
  (define (convert scale-kind rest-n tempo drums?)
    (define scale-root (select-from-list tone-names))
    (define scale (scale-kind scale-root))
    (convert-with scale rest-n tempo drums?))

  (let ()
    (local-require racket/pretty)
    (pretty-print c))

  (match-define (vector ts ap pattern parts) c)
  ;; xxx choose this (and make sure every instrument can play the notes)
  (define base-octave 3)

  (define (convert-with scale rest-n tempo drums?)
    (printf "Tempo is ~v\n" tempo)
    (chorded-song->commands*
     #:me
     (cons 0.25 tempo)
     #:ts ts
     #:drum
     ;; xxx generate this
     (cond
      [drums?
       (i:drums (vector i:drum:hihat
                        i:drum:bass
                        i:drum:snare))]
      [(not drums?)
       (i:drum (vector #f #f #f))]
      [else
       (i:drum (vector (select-from-list (list closed-hihat
                                               open-hihat
                                               loose-hihat))
                       bass-drum
                       (select-from-list (list snare-drum1
                                               snare-drum2))))])
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
                                 20 (spec:linear -5 5)
                                 05 (spec:linear 5 0)
                                 10 (spec:constant 0)
                                 05 (spec:linear 0 -5))
                      (spec:adsr 'sustain
                                 0 (spec:constant 0)
                                 0 (spec:constant 0)
                                 1 (spec:modulate 440.0 0 10)
                                 0 (spec:constant 0)))
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
     (convert scale-diatonic-major 8 480 #f)
     (convert scale-diatonic-major 8 280 #t)
     (convert scale-diminished #f 90 #t)
     (convert scale-harmonic-minor 16 210 #t)
     (convert (select-from-list scales)
              (and (zero? (random 2)) (+ 4 (random 8)))
              (+ 50 (random 400))
              #t)))

  (convert scale-diatonic-major 8 140 #t))

(provide composition->track
         nestration)
