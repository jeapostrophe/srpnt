#lang racket/base
(require srpnt/speaker
         srpnt/synth
         srpnt/mixer
         racket/match)

(define (play-to! m init-c)
  (define s (make-synth m))
  (let loop ([c init-c])
    (match c
      [(or #f '() (? void?))
       (void)]
      [(cons a d)
       (loop a)
       (loop d)]
      [(? synth:frame? f)
       (synth-step! s f)])))

(define (playing-mixer)
  (mixer:standard (speaker:real)))

(define (play-one! init-c)
  (define m (playing-mixer))
  (play-to! m init-c)
  (mixer-close! m))

(provide play-one!)
