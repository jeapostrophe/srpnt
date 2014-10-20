#lang racket/base
(require srpnt/music
         racket/match
         racket/list
         racket/fixnum
         racket/runtime-path)
(module+ test
  (require rackunit))

(define (cmd-length* c)
  (match c
    [(or #f '() (? void?))
     0]
    [(cons a d)
     (+ (cmd-length* a) (cmd-length* d))]
    [_
     1]))
(define-syntax-rule (cmd-length c)
  (printf "~a: ~v\n" 'c (cmd-length* c)))

(define-runtime-path clip-path "clip.raw.gz")
(define sample-bs (read-sample/gzip 0 4 clip-path))

(define bad-dudes-ex
  (cmd:hold* 10
             (cons
              (cmd:hold*f 40
                          (λ (f)
                            (cmd:frame* (wave:pulse (modulo f 3)
                                                    (pulse-freq->period 261.626)
                                                    7)
                                        #f #f #f #f #f)))
              (cmd:hold* 20
                         (cmd:frame* #f #f #f #f #f #f)))))

(define alien3-example
  (cmd:hold* 10
             (cons
              (for/list ([vol (in-list (list 0 7 6 5 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0))]
                         [dperiod (in-list (list -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 4 4 -3 -4 -4 -3 -4 -4 3 4 4 3 4 4 -3 -4 -4 -3 -4 -4 3 4 4 -3 -4 -4 -3))]
                         [duty (in-list (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1  0 0 0 0 0 0 0 0 1))])
                (cmd:frame* (wave:pulse duty
                                        (+ (pulse-freq->period 261.626) dperiod)
                                        vol)
                            #f #f #f #f #f))
              (cmd:hold* 20
                         (cmd:frame* #f #f #f #f #f #f)))))

(define gremlins2-example
  (cmd:hold* 10
             (list*
              (cmd:frame* (wave:pulse 2 (pulse-freq->period 261.626) 12)
                          #f #f #f #f #f)
              (cmd:hold* 3
                         (cmd:frame* (wave:pulse 1 (pulse-freq->period 261.626) 12)
                                     #f #f #f #f #f))
              (cmd:hold* 4
                         (cmd:frame* (wave:pulse 1 (pulse-freq->period 261.626) 11)
                                     #f #f #f #f #f))
              (cmd:hold* 4
                         (cmd:frame* (wave:pulse 1 (pulse-freq->period 261.626) 1)
                                     #f #f #f #f #f))
              (cmd:hold* 4
                         (cmd:frame* (wave:pulse 1 (pulse-freq->period 261.626) 3)
                                     #f #f #f #f #f))
              (cmd:hold* 16
                         (cmd:frame* #f #f #f #f #f #f)))))

(define noise-test-suite
  (for*/list ([short? (in-list (list #f #t))]
              [noise-p (in-range 16)])
    (cmd:hold* 15
               (cmd:frame* #f #f #f
                           (wave:noise short? noise-p 4)
                           #f #f))))

(define triangle-test-suite
  (for/list ([stp
              (for/list ([st (in-range -48 +83)])
                (hash-ref TRIANGLE st #f))]
             #:when stp)
    (cmd:hold* 15
               (cmd:frame* #f #f (wave:triangle #t stp) #f #f #f))))

(define pulse-test-suite
  (for/list ([stp
              (for/list ([st (in-range -48 +83)])
                (hash-ref PULSE st #f))]
             #:when stp)
    (for/list ([duty (in-list (list 0 1 2))])
      (cmd:hold* 15
                 (cmd:frame* (wave:pulse duty stp 4)
                             #f #f #f #f #f)))))

(define initial-test
  (list*
   (cmd:hold* 30
              (cmd:frame* (wave:pulse 0 (pulse-freq->period 261.626) 4)
                          #f #f #f #f #f))
   (cmd:hold* 30
              (cmd:frame* #f
                          (wave:pulse 2 (pulse-freq->period 440.00) 4)
                          #f #f #f #f))
   (cmd:hold* 15
              (cmd:frame* #f #f
                          (wave:triangle #t (triangle-freq->period 440.00))
                          #f #f #f))
   (cmd:hold* 15
              (cmd:frame* #f #f #f
                          (wave:noise #f 0 4)
                          #f #f))
   ;; XXX This is not a convenient interface, need a way to
   ;; fuse a DMC across the other frames
   (cmd:hold*f 55
               (λ (f)
                 (define s (fx* f samples-per-buffer))
                 (cmd:frame* #f #f #f #f
                             (wave:dmc sample-bs s)
                             #f)))))

(define silence
  (cmd:frame* #f #f #f #f #f #f))

;; 3, 4, 8 sound good
;; 9 is crunchy
;; 7 and C are okay

(define-syntax-rule (noise-line [len short? period vol] ...)
  (list* (cmd:hold* len (wave:noise short? period vol)) ...))

(define bass-drum
  (noise-line [1 #f 9 10] [2 #f 9 7] [3 #f 9 4] [3 #f 9 3] [4 #f 9 2]))
(module+ test
  (cmd-length bass-drum))

(define snare-drum1
  (noise-line [1 #f 7 11]
              [1 #f 7 9]
              [2 #f 7 8]
              [2 #f 7 7]
              [3 #f 7 4]
              [3 #f 7 3]
              [3 #f 7 2]))
(module+ test
  (cmd-length snare-drum1))

(define snare-drum2
  (noise-line [1 #f 7 11]
              [1 #f 7 9]
              [1 #f 7 8]
              [1 #f 7 7]
              [1 #f 7 6]
              [2 #f 7 4]
              [2 #f 7 3]
              [4 #f 7 2]))
(module+ test
  (cmd-length snare-drum2))

(define closed-hihat
  (noise-line [1 #f #xC 4]
              [2 #f #xC 3]
              [4 #f #xC 2]
              [4 #f #xC 1]))
(module+ test
  (cmd-length closed-hihat))

(define loose-hihat
  (noise-line [1 #f #xC 7]
              [2 #f #xC 5]
              [4 #f #xC 4]
              [2 #f #xC 2]
              [4 #f #xC 2]
              [2 #f #xC 1]))
(module+ test
  (cmd-length loose-hihat))

(define open-hihat
  (noise-line [1 #f #xC 6]
              [1 #f #xC 5]
              [3 #f #xC 4]
              [2 #f #xC 3]
              [4 #f #xC 2]
              [4 #f #xC 1]))
(module+ test
  (cmd-length open-hihat))

;; xxx this is ugly, it would be better to change the drum code to
;; take in a variable frame count
(define (ensure f c)
  (define clen (cmd-length* c))
  (if (fx> clen f)
      (take (flatten c) f)
      (cons c (cmd:hold* (max 0 (- f clen)) #f))))

;; xxx more real code

(require racket/flonum
         racket/contract/base)
(define note-factors
  (vector 1.0 2.0 4.0 8.0))
(define notes
  (apply vector
         (append*
          (for/list ([nf (in-vector note-factors)])
            (list
             ;; Dotted note
             ;; (fl* (fl/ 1.0 nf) 1.5)
             ;; Undotted note
             (fl/ 1.0 nf))))))
(define note/c
  (apply one-of/c (vector->list notes)))
(define metronome/c
  (cons/c note/c exact-nonnegative-integer?))

(define (frames-in-note me note)
  (match-define (cons beat-unit beats-per-minute) me)
  (define beats-per-second (fl/ (fx->fl beats-per-minute) 60.0))
  (define beats-per-frame (fl/ beats-per-second 60.0))
  (define frames-per-beat (fl/ 1.0 beats-per-frame))
  (define beats-in-note (fl/ note beat-unit))
  (define frames-in-note
    (fl* beats-in-note frames-per-beat))
  (fl->fx (flround frames-in-note)))
(module+ test
  (check-equal? (frames-in-note (cons 0.25 60) 0.25) 60)
  (check-equal? (frames-in-note (cons 0.25 60) 0.125) 30)
  (check-equal? (frames-in-note (cons 0.25 360) 0.25) 10)
  (check-equal? (frames-in-note (cons 0.25 1800) 0.125) 1)
  (check-equal? (frames-in-note (cons 0.25 3600) 0.25) 1)
  (check-equal? (frames-in-note (cons 0.25 3600) 0.125) 0))

(define time-sig/c
  (cons/c exact-nonnegative-integer?
          note/c))
(define ts:2:4 (cons 2 0.25))
(define ts:3:4 (cons 3 0.25))
(define ts:4:4 (cons 4 0.25))

(define (notes-in-bar ts)
  (match-define (cons beats-per-bar beat-unit) ts)
  (define notes-per-bar (fl* (fx->fl beats-per-bar) beat-unit))
  notes-per-bar)

(define (frames-in-bar me ts)
  (frames-in-note me (notes-in-bar ts)))
(module+ test
  (check-equal? (frames-in-bar (cons 0.25 60) (cons 4 0.25)) 240))

(define (ordered-bar-divisions ts)
  (define remainder->rhythms
    (make-hash))
  (define (divide bar)
    (hash-ref! remainder->rhythms
               bar
               (λ ()
                 (if (fl<= bar 0.0)
                     (list empty)
                     (append*
                      (for/list ([n (in-vector notes)])
                        (if (fl<= n bar)
                            (map (λ (l) (cons n l))
                                 (divide (fl- bar n)))
                            empty)))))))
  (divide (notes-in-bar ts)))
(module+ test
  (length (ordered-bar-divisions ts:4:4)))

(define (bar-divisions ts)
  (append*
   (map (λ (td)
          (remove-duplicates (permutations td)))
        (ordered-bar-divisions ts))))
(module+ test
  (length (bar-divisions ts:4:4)))

(require (only-in math/number-theory divisors))
(define (beat-accents ts)
  (match-define (cons beats-per-bar beat-unit) ts)
  (append*
   (for/list ([d (in-list (divisors beats-per-bar))])
     (for/list ([which (in-range d)])
       (for/list ([b (in-range beats-per-bar)])
         (fx= which (fxmodulo b d)))))))
(module+ test
  (beat-accents ts:2:4)
  (beat-accents ts:3:4)
  (beat-accents ts:4:4))

(define tone-names
  '(C C# D D# E F F# G G# A A# B))
(define tone->idx
  (for/hasheq ([t (in-list tone-names)]
               [i (in-naturals)])
    (values t i)))

(define (list-rotate base start)
  (define len (length base))
  (for/list ([i (in-range len)])
    (define new-i (fx+ start i))
    (define idx (fxmodulo new-i len))
    (define doctave (fxquotient new-i len))
    (match (list-ref base idx)
      [(? symbol? tone)
       (cons tone doctave)]
      [(cons tone doctave1)
       (cons tone (+ doctave1 doctave))])))

(define (scale-chromatic/idx start-idx)
  (list-rotate tone-names start-idx))
(define (scale-chromatic start-tone)
  (scale-chromatic/idx (hash-ref tone->idx start-tone)))
(module+ test
  (check-equal? (scale-chromatic/idx 0)
                '((C . 0) (C# . 0) (D . 0) (D# . 0) (E . 0) (F . 0)
                  (F# . 0) (G . 0) (G# . 0) (A . 0) (A# . 0) (B . 0)))
  (check-equal? (scale-chromatic 'C#)
                '((C# . 0) (D . 0) (D# . 0) (E . 0) (F . 0) (F# . 0)
                  (G . 0) (G# . 0) (A . 0) (A# . 0) (B . 0) (C . 1))))

(define (list-read l gaps)
  (match gaps
    ['()
     '()]
    [(cons gap gaps)
     (cons (first l)
           (list-read (list-tail l gap) gaps))]))

(define scales (list scale-chromatic))

(define (snoc l x) (append l (list x)))
(define-syntax-rule (define-scale scale-name offsets)
  (begin
    (define (scale-name start-tone)
      (define base (scale-chromatic start-tone))
      (list-read base offsets))
    (set! scales (snoc scales scale-name))))

(define-scale scale-diatonic-major '(2 2 1 2 2 2 1))
(module+ test
  (check-equal? (scale-diatonic-major 'C)
                '((C . 0) (D . 0) (E . 0) (F . 0) (G . 0) (A . 0) (B . 0)))
  (check-equal? (scale-diatonic-major 'E)
                '((E . 0) (F# . 0) (G# . 0) (A . 0) (B . 0) (C# . 1) (D# . 1))))

(define-scale scale-natural-minor '(2 1 2 2 1 2 2))
(define-scale scale-melodic-minor '(2 1 2 2 2 2 1))
(define-scale scale-harmonic-minor '(2 1 2 2 1 3 1))
(define-scale scale-diminished '(2 1 2 1 2 1 2 1))
(define-scale scale-whole-tone '(2 2 2 2 2 2))
(define-scale scale-blues '(3 2 1 1 3 2))
(define-scale scale-minor-pentatonic '(3 2 2 3 2))
(define-scale scale-major-pentatonic '(2 2 3 2 3))
(define-scale scale-hungarian-minor '(2 1 3 1 1 3 1))
(define-scale scale-persian '(1 3 1 1 2 3 1))
;; XXX only has 5 so messes with chords
;; (define-scale scale-hirojoshi '(2 1 4 1 4))
(define-scale scale-arabian '(2 2 1 1 2 2 2))
;; XXX only has 5 so messes with chords
;; (define-scale scale-scottish '(2 3 2 2 3))
;; xxx add exotic scales? http://www.lotusmusic.com/lm_exoticscales.html

(module+ test
  (for ([s (in-list scales)])
    (printf "~a ~a: ~a\n" s 'C (s 'C))))

(define (mode scale-tones start)
  (list-rotate scale-tones start))

(define modes empty)
(define-syntax-rule (define-mode mode-name start)
  (begin
    (define (mode-name scale) (mode scale start))
    (set! modes (snoc modes mode-name))))

(define-mode mode-ionian 0)
(define-mode mode-dorian 1)
(define-mode mode-phrygian 2)
(define-mode mode-lydian 3)
(define-mode mode-mixolydian 4)
(define-mode mode-aeolian 5)
(define-mode mode-locrian 6)

(module+ test
  (check-equal? (mode-ionian (scale-diatonic-major 'C))
                (scale-diatonic-major 'C))
  (check-equal? (mode-lydian (scale-diatonic-major 'C))
                '((F . 0) (G . 0) (A . 0) (B . 0) (C . 1) (D . 1) (E . 1))))

(define (modes-of scale-tones)
  (for/list ([i (in-range (length scale-tones))])
    (mode scale-tones i)))

(define (chord-triad scale-tones)
  (list-read scale-tones '(2 2 0)))
(module+ test
  (check-equal? (chord-triad (scale-diatonic-major 'C))
                '((C . 0) (E . 0) (G . 0))))

(define (chord-seventh scale-tones)
  (list-read scale-tones '(2 2 2 0)))
(module+ test
  (check-equal? (chord-seventh (scale-diatonic-major 'C))
                '((C . 0) (E . 0) (G . 0) (B . 0))))

(define (chord-sixth scale-tones)
  (list-read scale-tones '(2 2 1 0)))
(module+ test
  (check-equal? (chord-sixth (scale-diatonic-major 'C))
                '((C . 0) (E . 0) (G . 0) (A . 0))))

(define (all-chords chord-kind scale-tones)
  (map chord-kind (modes-of scale-tones)))

(module+ test
  (let ()
    (define (random-list-ref l)
      (list-ref l (random (length l))))
    (define start (random-list-ref tone-names))
    (define scale (random-list-ref scales))
    (define scale-tones (scale start))
    (printf "~a ~a: ~a\n" start scale scale-tones)
    (printf "Triads:\n")
    (for ([t (in-list (all-chords chord-triad scale-tones))])
      (printf "\t~a\n" t))
    (printf "Sevenths:\n")
    (for ([t (in-list (all-chords chord-seventh scale-tones))])
      (printf "\t~a\n" t))
    (printf "Sixths:\n")
    (for ([t (in-list (all-chords chord-sixth scale-tones))])
      (printf "\t~a\n" t))))

;; xxx add different kinds of accents
;; xxx generate drum beats
;;  - https://en.wikipedia.org/wiki/Drum_beat from accents & time-sigs
;;  - http://retrogameaudio.tumblr.com/post/19088836599/nes-audio-asterix-noise-instruments

;; instrument : bpm note tone -> semi-frames
;; concat : semi-frames ... -> semi-frames
;; overlay : semi-frames ... -> frames

(define (i:pulse duty volume)
  (λ (frames tone*accent?)
    (match-define (cons tone accent?) tone*accent?)
    (define evolume (if accent? (fxmin 15 (fx+ 1 volume)) volume))
    (cmd:hold* frames
               (wave:pulse duty (hash-ref PULSE tone) evolume))))

(define (i:triangle)
  (λ (frames tone)
    (cmd:hold* frames
               (wave:triangle #t (hash-ref TRIANGLE tone)))))

(define (i:pulse-plucky pluck% duty volume)
  (λ (frames tone*accent?)
    (match-define (cons tone accent?) tone*accent?)
    (define evolume (if accent? (fxmin 15 (fx+ 1 volume)) volume))
    (define per (hash-ref PULSE tone))
    (define pluck-frames (fl->fx (flceiling (fl* (fx->fl frames) pluck%))))
    (define unpluck-frames (fx- frames pluck-frames))
    (define unpluck-frames-third (fxquotient unpluck-frames 3))
    (define unpluck-frames-final-third
      (fx- unpluck-frames (fx* unpluck-frames-third 2)))
    (list*
     (cmd:hold* pluck-frames
                (wave:pulse duty per evolume))
     (cmd:hold* unpluck-frames-third
                (wave:pulse (fx- duty 1) per evolume))
     (cmd:hold* unpluck-frames-third
                (wave:pulse (fx- duty 1) per (fxquotient evolume 2)))
     (cmd:hold* unpluck-frames-final-third
                (wave:pulse (fx- duty 1) per (fxquotient evolume 4))))))

(define (i:pulse-slow-mod how-many duty volume)
  (λ (frames tone*accent?)
    (match-define (cons tone accent?) tone*accent?)
    (define evolume (if accent? (fxmin 15 (fx+ 1 volume)) volume))
    (define per (hash-ref PULSE tone))
    (define part-frames (fl->fx (flceiling (fl/ (fx->fl frames) (fx->fl how-many)))))
    (define-values (_ l)
      (for/fold ([remaining frames] [l empty])
                ([n (in-range how-many)])
        (values
         (fx- remaining part-frames)
         (cons l
               (cmd:hold* (fxmin remaining part-frames)
                          (wave:pulse (if (even? n) duty (fx- duty 1)) per evolume))))))
    l))

(define (i:drum which-v)
  (λ (frames which-n)
    (define which (vector-ref which-v which-n))
    (ensure frames which)))

(define (part->semicmds me p)
  (match-define (cons instru notes) p)
  ;; xxx it would be nice to remove this flatten
  (flatten
   (for/list ([n*t (in-list notes)])
     (match-define (cons note arg) n*t)
     (instru (frames-in-note me note) arg))))

(define list-#f (list #f))
(define (extend-with-#f v)
  (match v
    ['() list-#f]
    [(cons a d) v]))

(define (apply-mapish f ls)
  (match ls
    [(or '() (list '() ...))
     '()]
    [(list (cons a d)
           ...)
     (cons (f a)
           (apply-mapish f d))]
    [_
     ;; xxx When this is false, I add more to the end of one track
     ;; when it isn't balanced, but this has the problem of causing
     ;; empty patches. I think the real thing to do is ensure that
     ;; each track doesn't go under/over a measure's frames
     (if #t
         '()
         (apply-mapish f (map extend-with-#f ls)))]))

(define (combine-semicmds scs)
  ;; xxx this sucks
  (match scs
    [(list #f ...)
     (cmd:frame* #f #f #f #f #f #f)]
    [(list (? wave:pulse? p1) #f ...)
     (cmd:frame* p1 #f #f #f #f #f)]
    [(list (? wave:pulse? p1) (? wave:pulse? p2) #f ...)
     (cmd:frame* p1 p2 #f #f #f #f)]
    [(list (? wave:pulse? p1) (? wave:pulse? p2) (? wave:triangle? t) #f ...)
     (cmd:frame* p1 p2 t #f #f #f)]
    [(list (? wave:pulse? p1) (? wave:pulse? p2) (? wave:noise? n))
     (cmd:frame* p1 p2 #f n #f #f)]
    [(list (? wave:pulse? p1) (? wave:pulse? p2) (? wave:triangle? t) (? wave:noise? n))
     (cmd:frame* p1 p2 t n #f #f)]
    [(list #f #f (? wave:noise? n))
     (cmd:frame* #f #f #f n #f #f)]
    [(list #f #f #f (? wave:noise? n))
     (cmd:frame* #f #f #f n #f #f)]))

(define (combine-parts part-semicmds)
  (apply-mapish combine-semicmds part-semicmds))

(define (song->commands #:me me parts)
  (combine-parts (map (λ (p) (part->semicmds me p)) parts)))

(define main-track
  (cmd:repeat
   (cons
    (song->commands
     #:me (cons 0.25 160)
     (list
      (cons (i:pulse-plucky 0.25 2 8)
            (list (list* 0.25 'C4 #f)
                  (list* 0.25 'E4 #t)
                  (list* 0.25 'G4 #f)
                  (list* 0.25 'A4 #f)))
      (cons (i:pulse-slow-mod 16 2 4)
            (list (list* 0.25 'E4 #f)
                  (list* 0.25 'G4 #f)
                  (list* 0.25 'A4 #f)
                  (list* 0.25 'C4 #t)))
      (cons (i:triangle)
            (list (cons 0.25 'G3)
                  (cons 0.25 'A3)
                  (cons 0.25 'C3)
                  (cons 0.25 'E3)))
      (cons (i:drum (vector closed-hihat
                            bass-drum
                            snare-drum2))
            (list (cons 0.125 0)
                  (cons 0.125 0)
                  (cons 0.125 1)
                  (cons 0.125 0)
                  (cons 0.125 0)
                  (cons 0.125 0)
                  (cons 0.125 2)
                  (cons 0.125 0)))))
    (song->commands
     #:me (cons 0.25 160)
     (list
      (cons (i:pulse-plucky 0.25 2 8)
            (list (list* 0.25 'C4 #f)
                  (list* 0.25 'Eb4 #t)
                  (list* 0.25 'Gb4 #f)
                  (list* 0.25 'A4 #f)))
      (cons (i:pulse-slow-mod 16 2 4)
            (list (list* 0.25 'Eb4 #f)
                  (list* 0.25 'Gb4 #f)
                  (list* 0.25 'A4 #f)
                  (list* 0.25 'C4 #t)))
      (cons (i:triangle)
            (list (cons 0.25 'Gb3)
                  (cons 0.25 'A3)
                  (cons 0.25 'C3)
                  (cons 0.25 'Eb3)))
      (cons (i:drum (vector closed-hihat
                            bass-drum
                            snare-drum2))
            (list (cons 0.125 0)
                  (cons 0.125 0)
                  (cons 0.125 1)
                  (cons 0.125 0)
                  (cons 0.125 0)
                  (cons 0.125 0)
                  (cons 0.125 2)
                  (cons 0.125 0))))))))

(provide main-track)
