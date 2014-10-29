#lang racket/base
(require racket/list
         racket/fixnum
         racket/flonum
         racket/match
         srpnt/music-theory)
(module+ test
  (require rackunit))

(define (select-from-list l)
  (list-ref l (random (length l))))
(provide select-from-list)

(struct time-sig (name ts) #:transparent)
(define time-sig/ts:4:4 (time-sig "4/4" ts:4:4))
(define (select-time-sig)
  (select-from-list (list time-sig/ts:4:4)))

(struct accent-pattern (name pulses-per-measure accents) #:transparent)
(define (select-accent-pattern ts)
  (select-from-list
   (hash-ref
    (hash
     time-sig/ts:4:4
     (list (accent-pattern "standard"  1 '(#t #f #f #f))
           (accent-pattern "on-beats"  2 '(#t #f #t #f))
           (accent-pattern "off-beats" 2 '(#f #t #f #t))))
    ts)))

(define (accent-pattern-notes-per-pulse ap)
  (match-define (accent-pattern _ ppm as) ap)
  (/ (length as) ppm))

;; xxx https://en.wikipedia.org/wiki/Musical_form#Levels_of_organization

;; part-lens is in 4 measures
(struct form (name part-lens pattern) #:transparent)
(define (select-form)
  (select-from-list
   (list
    (form "strophic"
          '((A . 1))
          '(A))
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
;; xxx Should look at https://en.wikipedia.org/wiki/List_of_chord_progressions
;; From https://en.wikipedia.org/wiki/Chord_progression

;; xxx can't select one that is longer than notes/accents in part
;; xxx i should change this to allow me to write it roman numeral analysis
(define (select-chord-progression)
  (select-from-list
   (list
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

(define (apply-factors-and-ensure-sum total factors)
  (define-values (_rem _frag rresult)
    (for/fold ([rem total] [frag 0.0] [l empty])
              ([cd (in-list factors)]
               [i (in-naturals)])
      (define en (fl+ frag (exact->inexact (* cd total))))
      (define an (flmax 1.0 (flfloor en)))
      (define n
        (if (fx= i (fx- (length factors) 1))
            rem
            (fl->fx an)))
      (values (fx- rem n)
              (fl- en an)
              (cons n l))))
  (define result (reverse rresult))
  (unless (fx= total (sum result))
    (error 'apply-factors-and-ensure-sum
           "Inaccurate rounding (sum ~a) = ~a should be ~a"
           result (sum result) total))
  result)
(module+ test
  (check-equal? (apply-factors-and-ensure-sum 16 '(1/3 1/3 1/3))
                '(5 5 6))
  (check-equal? (apply-factors-and-ensure-sum 4 '(1/6 1/6 7/12 1/12))
                '(1 1 1 1)))

;; per chord phrase / measure

;; xxx select rhythm (needs to be compatible with accent pattern/time sig)
(define (select-rhythm ts notes)
  ;; xxx ignoring ts/ap
  (select-from-list
   (if #t
       (list
        (for/list ([k (in-range notes)])
          0.250))
       (list
        (let ()
          (define minimum-note 0.125)
          (define-values (rem l)
            (for/fold ([remaining (* 0.250 notes)]
                       [l empty])
                      ([k (in-range notes)])
              (define maximum-note
                (- remaining (* minimum-note (sub1 (- notes k)))))
              '(printf "MIN ~v REM ~v K ~v NS ~v MAX ~v\n"
                minimum-note remaining k notes maximum-note)
              (define this
                (if (= k (sub1 notes))
                    remaining
                    (select-from-list
                     (filter (λ (n) (<= n maximum-note))
                             (list 1.000 0.875 0.750 0.625
                                   0.500 0.375 0.250 0.125)))))
              (values (- remaining this)
                      (cons this l))))
          (unless (zero? rem)
            (error 'select-rhythm "Failed to use all notes: ~v vs ~v"
                   rem l))
          l)))))

;; xxx select chord note sequence of melody
;; xxx select matching notes of harmony
;; xxx select matching notes of bass

;; per play
;; xxx select bpm

(define (split-into-measures ts ns)
  ;; xxx generalize
  (let loop ([ns ns])
    (match ns
      [(list* a b c d more)
       (cons (list a b c d) (loop more))]
      ['() '()])))

(define (bithoven)
  (define scale lazy-scale)
  (define ts (select-time-sig))
  (define ap (select-accent-pattern ts))
  (define f (select-form))
  (define cp (select-chord-progression))
  (define cp-s (progression-seq cp))
  (define bns (select-bass-notes))
  (define btones
    (for/list ([bn (in-list bns)])
      (first (chord-triad (mode scale bn)))))
  (printf "~v\n"
          (vector f cp))
  (define measures-per-part
    (*
     ;; Every chord has to get one pulse at least (thus division) and
     ;; we need a balance of measures (thus ceiling)
     (let ()
       (ceiling
        (/ (length cp-s)
           (accent-pattern-pulses-per-measure ap))))
     ;; If a form is long, then don't make each part long
     (let ()
       (define pat-length (length (form-pattern f)))
       (cond
        [(< pat-length 3) 4]
        [(< pat-length 5) 2]
        [else 1]))))
  (define parts
    (for/hasheq ([p (in-list (form-part-lens f))])
      (match-define (cons label len) p)
      (define pd (select-progress-divison (length cp-s)))      
      (for ([a-pd (in-list pd)])
        (when (zero? a-pd)
          (error 'bithoven
                 "Cannot generate a part where a chord doesn't get any of division: ~v"
                 (vector cp-s pd))))
      (define pulses (* len measures-per-part (accent-pattern-pulses-per-measure ap)))
      (define chord-pulses
        (apply-factors-and-ensure-sum pulses pd))
      (for ([cp (in-list chord-pulses)])
        (when (zero? cp)
          (error 'bithoven
                 "Cannot generate a part where a chord doesn't get any pulses: ~v"
                 (vector pd chord-pulses))))
      (define chord-rhythm
        (for/list ([cp (in-list chord-pulses)])
          (select-rhythm ts (* cp (accent-pattern-notes-per-pulse ap)))))
      (printf "Rhythm: ~v x ~v => ~v\n" ts chord-pulses chord-rhythm)
      (define chord-track
        (split-into-measures
         ts
         (append*
          (for/list ([chord (in-list cp-s)]
                     [rhythm (in-list chord-rhythm)])
            ;; xxx don't always do triad?
            (define tones (chord-triad (mode scale chord)))
            (for/list ([r (in-list rhythm)])
              (define melody (select-from-list tones))
              (define harmony (select-from-list tones))
              (define allowed-bass-notes
                (filter (λ (t)
                          (memf (λ (ct) (eq? (car ct) (car t)))
                                tones))
                        btones))
              (when (empty? allowed-bass-notes)
                (error 'bithoven "No bass tone was found in ~v for ~v"
                       btones tones))
              (define bass
                (select-from-list allowed-bass-notes))
              ;; xxx correct accent
              (list* r (list melody harmony bass) #f))))))
      (values label
              chord-track)))
  (vector (time-sig-ts ts) (form-pattern f) parts))

(module+ test
  (require racket/pretty)
  (pretty-print (bithoven)))

(provide bithoven)
