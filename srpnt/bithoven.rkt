#lang racket/base
(require racket/list
         racket/fixnum
         racket/contract
         racket/flonum
         racket/match
         racket/promise
         srpnt/music-theory
         math/base
         data/enumerate
         data/enumerate/lib)
(module+ test
  (require rackunit))

(define-syntax-rule (λ/memo (arg ...) . body)
  (let ()
    (define MEMO (make-hash))
    (define (f arg ...)
      (define k (list* arg ...))
      (hash-ref! MEMO k
                 (λ () (let () . body))))
    f))
(define-syntax-rule (define/memo (f arg ...) . body)
  (define f (λ/memo (arg ...) . body)))

(struct time-sig (name ts) #:transparent)
(define time-sig/ts:4:4 (time-sig "4/4" ts:4:4))
(define time-sig/ts:3:4 (time-sig "3/4" ts:3:4))
(define time-sig/e
  (fin/e time-sig/ts:4:4))

;; FIXME #t means "accented" and "can be the start of a chord change",
;; but the second thing isn't correctly used
(struct accent-pattern (name pulses-per-measure accents) #:transparent)

(define time-sig->accents
  (hash
   time-sig/ts:4:4
   (list (accent-pattern "standard"  1 '(#t #f #f #f))
         (accent-pattern "on-beats"  2 '(#t #f #t #f))
         (accent-pattern "off-beats" 2 '(#f #t #f #t)))
   time-sig/ts:3:4
   (list (accent-pattern "waltz"  1 '(#t #f #f)))))

(define (accent-pattern/e ts)
  (apply fin/e (hash-ref time-sig->accents ts)))

(define (accent-pattern-notes-per-pulse ap)
  (match-define (accent-pattern _ ppm as) ap)
  (/ (length as) ppm))

;; part-lens is in 4 measures
(struct form (name part-lens pattern) #:transparent)
(define form/e
  (fin/e
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
         '(A B A B C A B C A B))))

(struct progression (seq) #:transparent)

;; From https://en.wikipedia.org/wiki/Chord_progression
(define chord-progression/e
  (fin/e
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
   ;; 50s
   (progression '(0 3 4))
   (progression '(0 5 3 4))
   ;; circle
   (progression '(5 1 4 0))
   (progression '(0 4 0))
   (progression '(0 3 4 0))
   (progression '(0 5 1 4))
   ;; harmonizing
   (progression '(0 1 2 3 4))
   (progression '(0 1 0 3 4))
   ;; andalusian
   (progression '(0 6 5 4))
   (progression '(0 2 3 5))))

(define bass-notes/e
  (fin/e (list 0 3 4)))

(define (chord-pulses/e pulse-count chord-count)
  (list-of-length-n-summing-to-k-with-no-zeros/e chord-count pulse-count))

(define/memo (list-of-length-n-summing-to-k-with-no-zeros/e n k)
  (unless (and (positive? n)
               (positive? k)
               (<= n k))
    (error 'list-of-length-n-summing-to-k-with-no-zeros/e
           "Illegal arguments: ~a and ~a"
           n k))
  (cond
    [(= 1 n)
     (single/e (list k))]
    [else
     (dep/e
      #:one-way? #f
      (map/e add1 sub1
             ;; xxx really this number is restricted in range
             #:contract number?
             (below/e (+ 1 (- k n))))
      #:f-range-finite? #t
      #:flat? #t
      (λ (this)
        (list-of-length-n-summing-to-k-with-no-zeros/e
         (- n 1)
         (- k this))))]))

(module+ test
  (printf "16 x 3 =\n")
  (define e (chord-pulses/e 16 3))
  (for ([i (in-naturals)]
        [pd (in-enum e)])
    (printf "~a. ~a\n" i pd)))

(module+ test
  (for* ([k (in-range 1 10)]
         [n (in-range k 10)])
    (define e (chord-pulses/e n k))
    (printf "(f ~a ~a) = ~a\n"
            n k (enum-count e))))

(define chord-kinds
  (vector chord-triad chord-seventh chord-sixth))
(define chord-kind/e
  (below/e (vector-length chord-kinds)))

(define tones/e
  (vector/e chord-kind/e (below/e 4) (below/e 4) (below/e 4) (below/e 4)))

(define (note/e beat-unit len)
  (cons/e (single/e (* len beat-unit)) tones/e))
(define note-rep/c
  ;; xxx really this real is restricted somewhat
  (cons/c real? (enum-contract tones/e)))

(define/memo (maybe-combine/e a-ts notes)
  (match-define (time-sig _ ts) a-ts)
  (match-define (cons beats-per-bar beat-unit) ts)
  (if (notes . <= . 1)
      (dont-combine/e a-ts notes)
      (cons/e
       (dep/e
        #:one-way? #f
        bool/e
        #:flat? #t
        #:f-range-finite? #t
        (λ (combine?)
          (if combine?
              (list/e (note/e beat-unit 1) (note/e beat-unit 1))
              (list/e (note/e beat-unit 2)))))
       (maybe-combine/e a-ts (- notes 2)))))
(define/memo (dont-combine/e a-ts notes)
  (match-define (time-sig _ ts) a-ts)
  (match-define (cons beats-per-bar beat-unit) ts)
  (if (zero? notes)
      (single/e '())
      (cons/e (list/e (note/e beat-unit 1))
              (maybe-combine/e a-ts (sub1 notes)))))

(define/memo (rhythm/e a-ts notes)
  (maybe-combine/e a-ts notes))

(module+ test
  (printf "r 4:4 4 =\n")
  (define re (rhythm/e time-sig/ts:4:4 4))
  (for ([i (in-range 100)]
        [r (in-enum re)])
    (printf "~a. ~a\n" i r)))

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

(define (list-mref l i)
  (list-ref l (modulo i (length l))))

(define (bithoven->composition bi)
  (match-define
    (vector (cons ts (cons ap (cons (cons f cp) cps*crs-s))) bns)
    bi)
  (define scale lazy-scale)
  (define cp-s (progression-seq cp))
  (define parts
    (for/hasheq ([p (in-list (form-part-lens f))]
                 [chord-pulses*chord-rhythm (in-list cps*crs-s)])
      (match-define (cons chord-pulses chord-rhythm) chord-pulses*chord-rhythm)
      (match-define (cons label len) p)
      (define chord-track
        (split-into-measures
         ts ap
         (append*
          (for/list ([chord (in-list cp-s)]
                     [rhythm (in-list chord-rhythm)])
            (for/list ([r-info (in-list (filter cons? (append* rhythm)))])
              (match-define (cons r (vector chord-kind-idx melody-idx harmony-idx
                                            tenor-idx bass-idx))
                r-info)
              (define chord-kind (vector-ref chord-kinds chord-kind-idx))
              (define tones (chord-kind (mode scale chord)))
              (define melody (list-mref tones melody-idx))
              (define harmony (list-mref tones harmony-idx))              
              (define tenor (list-mref tones tenor-idx))
              (define bass (list-mref tones bass-idx))
              (vector r (list melody harmony tenor bass)))))))
      (values label
              chord-track)))
  (vector (time-sig-ts ts) (accent-pattern-accents ap) (form-pattern f) parts))

(define/memo (part/e ts ap cp measures len)
  (define cp-s (progression-seq cp))
  (define pulses
    (* len measures (accent-pattern-pulses-per-measure ap)))
  (define cp/e
    (chord-pulses/e
     pulses
     (length cp-s)))
  (dep/e
   #:one-way? #f
   #:flat? #t
   #:f-range-finite? #t
   cp/e
   (λ (cps)
     (traverse/e
      (λ (cp)
        (rhythm/e ts (* cp (accent-pattern-notes-per-pulse ap))))
      cps))))

(define (traverse/e f l)
  (apply list/e (map f l)))

(define p:bithoven/e
  (delay
    (vector/e
     (dep/e
      #:one-way? #f
      #:flat? #t
      #:f-range-finite? #t
      time-sig/e
      (λ (ts)
        (dep/e
         #:one-way? #f
         #:flat? #t
         #:f-range-finite? #t
         (accent-pattern/e ts)
         (λ (ap)
           (dep/e
            #:one-way? #f
            #:flat? #t
            #:f-range-finite? #t
            (cons/e form/e chord-progression/e)
            (λ (f*cp)
              (match-define (cons f cp) f*cp)
              (define cp-s (progression-seq cp))
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
              (define/memo (this-kind-of-part/e len)
                (part/e ts ap cp measures-per-part len))
              (traverse/e
               (λ (p) (this-kind-of-part/e (cdr p)))
               (form-part-lens f))))))))
     bass-notes/e)))

(provide bithoven->composition
         p:bithoven/e)
