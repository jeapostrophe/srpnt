#lang racket/base
(require data/enumerate
         data/enumerate/lib)

(define (random-index/printing e)
  (define n (random-index e))
  (define k (size e))
  (local-require racket/format)
  (define ks (~a k))
  (define ns (~a #:min-width (string-length ks) #:align 'right n))
  (printf "Using n =\n\t~a of\n\t~a\n\n" ns ks)
  n)

(provide random-index/printing)
