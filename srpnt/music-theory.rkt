#lang racket/base
(require racket/list
         racket/match
         racket/fixnum
         racket/flonum
         racket/contract/base)
(module+ test
  (require rackunit))

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

(define (list-rotate/modify modify base start)
  (define len (length base))
  (for/list ([i (in-range len)])
    (define new-i (fx+ start i))
    (define idx (fxmodulo new-i len))
    (define doctave (fxquotient new-i len))
    (modify (list-ref base idx) doctave)))

(define (list-rotate base start)
  (define (modify v doctave)
    (match v
      [(? symbol? tone)
       (cons tone doctave)]
      [(cons tone doctave1)
       (cons tone (+ doctave1 doctave))]))
  (list-rotate/modify modify base start))

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
;; xxx messes with chords
;; (define-scale scale-blues '(3 2 1 1 3 2))
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

(define (chord-inversion chord-tones start)
  (list-rotate/modify
   (λ (v d) v)
   chord-tones
   (fxmodulo start (length chord-tones))))

(define (chord-inversions chord-tones)
  (for/list ([start (in-range (length chord-tones))])
    (chord-inversion chord-tones start)))

(define (octave-delta tones delta)
  (for/list ([t (in-list tones)])
    (match-define (cons tn do) t)
    (cons tn (+ delta do))))

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
      (printf "\t~a\n" t))
    (printf "Triad Inversions:\n")
    (for ([t (in-list (chord-inversions (chord-triad scale-tones)))])
      (printf "\t~a\n" t))
    (printf "Seventh Inversions:\n")
    (for ([t (in-list (chord-inversions (chord-seventh scale-tones)))])
      (printf "\t~a\n" t))))

;; xxx
(provide (all-defined-out))
