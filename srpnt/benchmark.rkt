#lang racket/base
(require racket/promise
         racket/port
         data/enumerate/lib)

(define-syntax-rule (time-it label e)
  (let* ([before (current-inexact-milliseconds)]
         [ans e]
         [after (current-inexact-milliseconds)])
    (printf "~a: ~ams\n" label (- after before))
    ans))

(define-syntax-rule (define/dyn i m)
  (define i (dynamic-require 'm 'i)))

(module+ main
  (time-it "loading" (dynamic-require 'srpnt/studio #f))
  (define p:b/e
    (time-it "loading bit" (dynamic-require 'srpnt/bithoven 'p:bithoven/e)))
  (define/dyn bithoven->composition srpnt/bithoven)
  (define b/e
    (time-it "construct enum" (force p:b/e)))
  (define N 100)
  (define bis
    (time-it (format "~a composition scripts" N)
             (for/list ([i (in-range N)]) (from-nat b/e (random-index b/e)))))
  (define cs
    (time-it (format "~a compositions" N)
             (for/list ([bi (in-list bis)])
               (bithoven->composition bi))))
  (define ns
    (time-it (format "~a NEStrations" N)
             (let ()
               (define/dyn make-nestration/e srpnt/nestration)
               (for/list ([c (in-list cs)])
                 (define n/e (make-nestration/e c))
                 (from-nat n/e (random-index n/e))))))
  (define ss
    (time-it (format "~a songs" N)
             (let ()
               (define/dyn compile-song srpnt/band)
               (parameterize ([current-output-port (open-output-nowhere)])
                 (for/list ([c (in-list cs)]
                            [n (in-list ns)])
                   (compile-song c n))))))
  (define as
    (time-it (format "~a audio" N)
             (let ()
               (define/dyn play-one! srpnt/player)
               (define/dyn mixer:standard srpnt/mixer)
               (define/dyn speaker:null srpnt/speaker)
               (define the-m (mixer:standard (speaker:null)))
               (define (m) the-m)
               (for ([s (in-list ss)])
                 (play-one! #:mixer m s)))))
  42)
