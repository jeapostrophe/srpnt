#lang racket/base
(require racket/contract/base)

(require "speaker/h.rkt")
(provide (all-from-out "speaker/h.rkt"))

(require "speaker/portaudio.rkt")

(define (speaker:real)
  (define bp (make-bytes-player))
  (speaker
   (λ (out-bs)
     (bytes-play! bp out-bs))
   (λ ()
     (close-bytes-player! bp))))

(provide
 (contract-out
  [speaker:real (-> speaker?)]))
