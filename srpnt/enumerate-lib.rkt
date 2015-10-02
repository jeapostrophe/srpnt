#lang racket/base
(require data/enumerate
         data/enumerate/lib)

(define (old-fin/e . args-l)
  (define args-v (list->vector args-l))
  (define (f idx) (vector-ref args-v idx))
  (define (f-inv arg)
    (for/or ([idx (in-naturals)]
             [ARG (in-vector args-v)]
             #:when (equal? arg ARG))
      idx))
  (map/e f f-inv
         #:contract (λ (x) (not (boolean? (f-inv x))))
         (below/e (vector-length args-v))))

(provide old-fin/e)
