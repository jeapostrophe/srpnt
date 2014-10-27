#lang racket/base
(require racket/list
         racket/match
         srpnt/music-theory)
(module+ test
  (require rackunit))

(define (select-from-list l)
  (list-ref l (random (length l))))

(struct time-sig (name ts) #:transparent)
(define time-sig/ts:4:4 (time-sig "4/4" ts:4:4))
(define (select-time-sig)
  (select-from-list (list time-sig/ts:4:4)))

(struct accent-pattern (name accents) #:transparent)
(define (select-accent-pattern ts)
  (select-from-list
   (hash-ref
    (hash
     time-sig/ts:4:4
     (list (accent-pattern "standard"     '(#t #f #f #f))
           (accent-pattern "on-beats"     '(#t #f #t #f))
           (accent-pattern "off-beats"    '(#f #t #f #t))
           (accent-pattern "on-off-beats" '(#t #f #t #f #f #t #f #t))))
    ts)))

;; xxx https://en.wikipedia.org/wiki/Musical_form#Levels_of_organization

;; part-lens is in 4 measures
(struct form (name part-lens pattern) #:transparent)
(define (select-form)
  (select-from-list
   (list (form "strophic"
               '((A . 1))
               '(A A A A))
         (form "medley"
               '((A . 1) (B . 1) (C . 1) (D . 1))
               '(A B C D))
         (form "double medley"
               '((A . 1) (B . 1) (C . 1) (D . 1))
               '(A A B B C C D D))
         (form "binary"
               '((A . 1) (B . 1))
               '(A B))
         (form "double binary"
               '((A . 1) (B . 1))
               '(A A B B))
         (form "repeated binary"
               '((A . 1) (B . 1))
               '(A B A B))
         (form "ternary"
               '((A . 1) (B . 1))
               '(A B A))
         (form "repeated ternary"
               '((A . 1) (B . 1))
               '(A A B A))
         (form "asym rondo"
               '((A . 1) (B . 1) (C . 1) (D . 1) (E . 1))
               '(A B A C A D A E A))
         (form "sym rondo"
               '((A . 1) (B . 1) (C . 1))
               '(A B A C A B A))
         (form "sym rondo"
               '((A . 1) (B . 1) (C . 1))
               '(A B A C A B A))
         (form "arch"
               '((A . 1) (B . 1) (C . 1))
               '(A B C B A))
         (form "typical pop"
               '((I . 1) (V . 1) (C . 1) (M8 . 2) (O . 1))
               '(I V C V C M8 C C O))
         (form "32-bar"
               '((A . 2) (B . 2))
               '(A A B A))
         (form "AAA"
               '((A . 1))
               '(A A A))
         (form "ABABCB"
               '((A . 1) (B . 1) (C . 1))
               '(A B A B C B))
         (form "ABABCAB"
               '((A . 1) (B . 1) (C . 1))
               '(A B A B C A B))
         (form "ABABCBAB"
               '((A . 1) (B . 1) (C . 1))
               '(A B A B C B A B))
         (form "ABABCABCAB"
               '((A . 1) (B . 1) (C . 1))
               '(A B A B C A B C A B)))))

(struct progression (seq) #:transparent)
;; Should look at https://en.wikipedia.org/wiki/List_of_chord_progressions
;; From https://en.wikipedia.org/wiki/Chord_progression

;; xxx can't select one that is longer than notes/accents in part
(define (select-chord-progression)
  (select-from-list
   (list
    ;; simple
    ;; (progression '(0))
    ;; (progression '(1))
    ;; (progression '(2))
    ;; (progression '(3))
    ;; (progression '(4))
    ;; (progression '(5))
    ;; (progression '(6))
    ;; three chord
    (progression '(0 3 4 4))
    (progression '(0 0 3 4))
    (progression '(0 3 0 4))
    (progression '(0 3 4 3))
    ;; three chord with minor subst
    (progression '(0 2 4 4))
    (progression '(0 0 2 4))
    (progression '(0 2 0 4))
    (progression '(0 2 4 3))
    (progression '(0 3 4 2))
    (progression '(0 1 4))
    (progression '(1 4 0))
    (progression '(0 3 4 4 0 3 4 0))
    ;; blues
    (progression '(0 0 0 0 3 3 0 0 4 3 0 0))
    ;; 50s
    (progression '(0 3 4))
    (progression '(0 5 3 4))
    (progression '(0 5 1 4))
    ;; circle
    (progression '(5 1 4 0))
    (progression '(0 3 6 2 5 1 4 0))
    (progression '(0 4 0))
    (progression '(0 3 4 0))
    (progression '(0 5 1 4))
    ;; harmonizing
    (progression '(0 1 2 3 4))
    (progression '(0 1 0 3 4))
    ;; andalusian
    (progression '(0 6 5 4))
    (progression '(0 2 3 5)))))

(define (select-bass-notes)
  (list 0 3 4))

;; per part
(define (random-between lo hi)
  (cond
   [(= lo hi) lo]
   [else
    (unless (> hi lo)
      (error 'random-between "~v should be larger than ~v" hi lo))
    (+ lo (random (- hi lo)))]))

(define (sum l)
  (foldr + 0 l))

;; xxx actually should be compatible with accent pattern/time sig
;; xxx this could be better defined by selecting a permutation
(define (select-progress-divison chords)
  (select-from-list
   (list
    ;; Equal
    (for/list ([k (in-range chords)])
      (/ 1 chords))
    ;; Random
    (let ()
      (define factors (* (random-between 1 4) chords))
      (define-values (_ l)
        (for/fold ([remaining factors]
                   [l empty])
                  ([k (in-range chords)])
          (define this
            (if (= k (sub1 chords))
                remaining
                (random-between 1 (- remaining (sub1 (- chords k))))))
          (values (- remaining this)
                  (cons (/ this factors) l))))
      (unless (= 1 (sum l))
        (error 'select-progress-divison "Division didn't sum to 1: ~v" l))
      l))))

(define (apply-factors-and-ensure-sum total l)
  (define-values (_ result)
    (for/fold ([off 0] [l empty])
              ([cd (in-list l)])
      (define n (* cd total))
      (define nr (ceiling n))
      (cond
       [(= n nr)
        (values off (cons nr l))]
       [else
        (define rn (+ (* cd total) off))
        (define rnr (ceiling rn))
        (define diff (- rn rnr))
        (values diff (cons rnr l))])))
  (unless (= total (sum result))
    (error 'apply-factors-and-ensure-sum
           "Inaccurate rounding (sum ~a) = ~a should be ~a"
           result (sum result) total))
  result)
(module+ test
  (check-equal? (apply-factors-and-ensure-sum 16 '(1/3 1/3 1/3))
                '(5 5 6)))

;; per chord phrase / measure

;; xxx select rhythm (needs to be compatible with accent pattern/time sig)
(define (select-rhythm ts notes)
  ;; xxx assuming 4/4
  (for/list ([i (in-range notes)])
    0.250))

;; xxx select chord note sequence of melody
;; xxx select matching notes of harmony
;; xxx select matching notes of bass

;; per play
;; xxx select bpm

(define (bithoven)
  ;; xxx really a play parameter
  (define scale-kind (select-from-list scales))
  (define scale-root (select-from-list tone-names))
  (define scale (scale-kind scale-root))
  (define ts (select-time-sig))
  (define ap (select-accent-pattern ts))
  (define f (select-form))
  (define cp (select-chord-progression))
  (define cp-s (progression-seq cp))
  (define bns (select-bass-notes))
  (define btones
    (for/list ([bn (in-list bns)])
      (first (chord-triad (mode scale bn)))))
  (define parts
    (for/list ([p (in-list (form-part-lens f))])
      (match-define (cons label len) p)
      (define pd (select-progress-divison (length cp-s)))
      ;; xxx could use the length of the progression instead of 4
      (define notes (* len 4 (length (accent-pattern-accents ap))))
      (define chord-notes
        (apply-factors-and-ensure-sum notes pd))
      (define chord-rhythm
        (for/list ([c (in-list chord-notes)])
          (select-rhythm ts c)))
      (define chord-track
        (for/list ([chord (in-list cp-s)]
                   [rhythm (in-list chord-rhythm)])
          (define tones (chord-triad (mode scale chord)))
          (vector chord tones
                  (for/list ([r (in-list rhythm)])
                    (define melody (select-from-list tones))
                    (define harmony (select-from-list tones))
                    (define bass
                      (select-from-list
                       (filter (λ (t)
                                 (memf (λ (ct) (eq? (car ct) (car t)))
                                       tones))
                               btones)))
                    (unless bass
                      (error 'bithoven "No bass tone was found in ~v for ~v"
                             btones tones))
                    (vector r melody harmony bass)))))
      (vector label
              pd
              notes
              chord-notes
              chord-rhythm
              chord-track)))
  (vector scale-kind scale-root ts ap f cp bns btones parts))

(module+ test
  (require racket/pretty)
  (pretty-print (bithoven)))
