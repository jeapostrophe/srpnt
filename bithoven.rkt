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

;; xxx #t means "accented" and "can be the start of a chord
;; change", but the second thing isn't correctly used
(struct accent-pattern (name pulses-per-measure accents) #:transparent)

(define time-sig->accents
  (hash
   time-sig/ts:4:4
   (list (accent-pattern "standard"  1 '(#t #f #f #f))
         (accent-pattern "on-beats"  2 '(#t #f #t #f))
         (accent-pattern "off-beats" 2 '(#f #t #f #t)))))

(define (select-accent-pattern ts)
  (select-from-list
   (hash-ref
    time-sig->accents
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
      (define an (flmax 1.0
                        (flmin (flfloor en)
                               (fx->fl (fx- rem (fx- (length factors) i))))))
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
                '(1 1 1 1))
  (check-equal? (apply-factors-and-ensure-sum 4 '(1/4 1/2 1/8 1/8))
                '(1 1 1 1)))

;; xxx (needs to be compatible with accent pattern)
(define (select-rhythm a-ts notes)
  (match-define (time-sig _ ts) a-ts)
  (match-define (cons beats-per-bar beat-unit) ts)
  (select-from-list
   (list
    (for/list ([k (in-range notes)])
      beat-unit)
    (let loop ([remaining (* beat-unit notes)])
      (cond
       [(fl= 0.0 remaining)
        empty]
       [else
        (define-values (next-total nexts)
          (if (<= (* 2 beat-unit) remaining)
              (if (zero? (random 2))
                  (values (* 2 beat-unit) (list (* 2 beat-unit)))
                  (values (* 2 beat-unit) (list beat-unit beat-unit)))
              (values beat-unit (list beat-unit))))
        (append nexts (loop (fl- remaining next-total)))])))))

(define (split-into-measures a-ts ap ns)
  (match-define (time-sig _ ts) a-ts)
  (match-define (cons beats-per-bar beat-unit) ts)
  (define goal (notes-in-bar ts))
  (match-define (accent-pattern _ _ accents) ap)
  (define (consume-notes ns)
    (let loop ([total 0.0] [so-far empty] [ns ns])
      (cond
       [(> total goal)
        (error 'split-into-measures "Notes went over a measure!")]
       [(= total goal)
        (values (reverse so-far) ns)]
       [else
        (match-define (vector note args) (first ns))
        (define new-total (fl+ note total))
        (define (list-ref* l i)
          (if (<= (length l) i)
              #f
              (list-ref l i)))
        (define accented?
          (list-ref* accents (fl->fx (flceiling (fl/ total beat-unit)))))
        (define new-note (list* note args accented?))
        (define new-so-far (cons new-note so-far))
        (loop new-total new-so-far (rest ns))])))
  (define (consume-measures ns)
    (match ns
      ['() '()]
      [_
       (define-values (a-measure more-ns) (consume-notes ns))
       (cons a-measure (consume-measures more-ns))]))
  (consume-measures ns))

(module+ test
  (let ()
    (define ts time-sig/ts:4:4)
    (check-equal?
     (split-into-measures ts (first (hash-ref time-sig->accents ts))
                          (list (vector 0.25 'N)
                                (vector 0.25 'N)
                                (vector 0.25 'N)
                                (vector 0.25 'N)

                                (vector 0.25 'N)
                                (vector 0.25 'N)
                                (vector 0.25 'N)
                                (vector 0.25 'N)))
     (list (list (list* 0.25 'N #t)
                 (list* 0.25 'N #f)
                 (list* 0.25 'N #f)
                 (list* 0.25 'N #f))
           (list (list* 0.25 'N #t)
                 (list* 0.25 'N #f)
                 (list* 0.25 'N #f)
                 (list* 0.25 'N #f))))

    (check-equal?
     (split-into-measures ts (first (hash-ref time-sig->accents ts))
                          (list (vector 0.125 'N) (vector 0.125 'N)
                                (vector 0.125 'N) (vector 0.125 'N)
                                (vector 0.125 'N) (vector 0.125 'N)
                                (vector 0.125 'N) (vector 0.125 'N)))
     (list (list (list* 0.125 'N #t) (list* 0.125 'N #f)
                 (list* 0.125 'N #f) (list* 0.125 'N #f)
                 (list* 0.125 'N #f) (list* 0.125 'N #f)
                 (list* 0.125 'N #f) (list* 0.125 'N #f))))

    (check-exn
     exn:fail?
     (λ ()
       (split-into-measures ts (first (hash-ref time-sig->accents ts))
                            (list (vector 0.125 'N) (vector 0.125 'N)
                                  (vector 0.125 'N) (vector 0.125 'N)
                                  (vector 0.125 'N) (vector 0.125 'N)
                                  (vector 0.125 'N)))))
    (check-exn
     exn:fail?
     (λ ()
       (split-into-measures ts (first (hash-ref time-sig->accents ts))
                            (list (vector 0.125 'N) (vector 0.125 'N)
                                  (vector 0.125 'N) (vector 0.125 'N)
                                  (vector 0.125 'N) (vector 0.125 'N)
                                  (vector 0.125 'N) (vector 0.250 'N)))))))

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
     4
     #;
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
         ts ap
         (append*
          (for/list ([chord (in-list cp-s)]
                     [rhythm (in-list chord-rhythm)])
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
              (vector r (list melody harmony bass)))))))
      (values label
              chord-track)))
  (vector (time-sig-ts ts) (accent-pattern-accents ap) (form-pattern f) parts))

(module+ test
  (require racket/pretty)
  (pretty-print (bithoven)))

(provide bithoven)
