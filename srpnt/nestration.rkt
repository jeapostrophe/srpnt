#lang racket/base
(require racket/match
         racket/list
         racket/contract
         racket/fixnum
         data/enumerate/lib
         srpnt/enumerate-lib
         srpnt/music-theory
         srpnt/nestration/instruments)

(define tone-names/e
  (apply old-fin/e tone-names))

(define scales/e (apply fin/e scales))
(define tempo/e (range/e 40 500))

(define rest-n/e
  (fin/e #f 4 5 6 7 8 9 10 11 12 13 14 15 16))

(define pulse/e (apply old-fin/e is:pulses))
(define triangle/e (apply old-fin/e is:triangles))
(define drums/e (apply old-fin/e is:drums))

(define mhtb/e
  (permutations-of-n/e 4))

(define (drum-measure/e ts ap)
  (cond
   [(eq? ts ts:4:4)
    (apply old-fin/e
           beats:4/4)]
   [(eq? ts ts:3:4)
    (apply old-fin/e
           beats:3/4)]))

(struct style
  (name tone-names/e scales/e tempo/e
        pulse1/e pulse2/e
        triangle1/e triangle2/e
        drums/e mhtb/e))

(define-syntax-rule (define-styles styles [id . expr] ...)
  (begin (define id (style . expr)) ...
         (define styles (list id ...))
         (provide style-name styles id ...)))

(define-styles styles
  [style:classic
   "Classic"
   tone-names/e (old-fin/e scale-diatonic-major) (range/e 160 300)
   (apply old-fin/e is:pulses-classic)
   (apply old-fin/e is:pulses-classic)
   (old-fin/e i:triangle:basic i:triangle:plucky)
   (old-fin/e i:triangle:basic i:triangle:plucky)
   (old-fin/e i:drums:basic)
   (old-fin/e (list 0 1 2 3))]
  [style:happy
   "Happy"
   tone-names/e (old-fin/e scale-diatonic-major) (fin/e 200)
   pulse/e pulse/e triangle/e triangle/e drums/e (old-fin/e (list 0 1 2 3))]
  [style:sad
   "Sad"
   tone-names/e (old-fin/e scale-harmonic-minor) (fin/e 120)
   pulse/e pulse/e triangle/e triangle/e drums/e mhtb/e]
  [style:all
   "ALL"
   tone-names/e scales/e tempo/e pulse/e pulse/e triangle/e triangle/e drums/e mhtb/e])

(define (make-nestration/e
         #:style [style style:all]
         #:tone-names/e [tone-names/e (style-tone-names/e style)]
         #:scales/e [scales/e (style-scales/e style)]
         #:tempo/e [tempo/e (style-tempo/e style)]
         #:pulse1/e [pulse1/e (style-pulse1/e style)]
         #:pulse2/e [pulse2/e (style-pulse2/e style)]
         #:triangle1/e [triangle1/e (style-triangle1/e style)]
         #:triangle2/e [triangle2/e (style-triangle2/e style)]
         #:drums/e [drums/e (style-drums/e style)]
         #:mhtb/e [mhtb/e (style-mhtb/e style)]
         c)
  (match-define (vector ts ap pattern parts) c)
  (vector/e tone-names/e scales/e tempo/e
            pulse1/e pulse2/e triangle1/e triangle2/e drums/e
            mhtb/e
            (fin/e 2) (fin/e 1) (fin/e 1) (fin/e 1)
            ;; (fin/e 2 3) (fin/e 1 2) (fin/e 0 2) (fin/e 1 2)
            (hash-traverse/e
             #:get-contract (λ (x) (listof (listof (cons/c real? boolean?))))
             (λ (_) (drum-measure/e ts ap))
             parts)
            (hash-traverse/e
             #:get-contract (λ (x) (listof exact-nonnegative-integer?))
             (λ (ms)
               (dep/e rest-n/e
                      #:f-range-finite? #t
                      (λ (rest-n)
                        (if rest-n
                            (listof-n/e
                             (below/e rest-n)
                             (add1 (ceiling (/ (length (append* ms)) rest-n))))
                            (single/e '())))))
             parts)))

;; xxx contracts
(provide (struct-out style)
         make-nestration/e)
