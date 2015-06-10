#lang racket/base
(require portaudio/portaudio
         portaudio/devices
         racket/match
         racket/flonum
         racket/contract/base
         "h.rkt")

(define reasonable-latency (fl/ 1.0 60.0))

(struct bp (stream buf))

(define (make-bytes-player)
  (pa-maybe-initialize)
  (define device-number (find-output-device reasonable-latency))
  (define device-latency (device-low-output-latency device-number))
  (define output-stream-parameters
    (make-pa-stream-parameters
     device-number channels
     '(paUInt8)
     device-latency #f))
  (define stream
    (pa-open-stream
     #f output-stream-parameters
     sample-rate.0 samples-per-buffer
     null #f #f))
  (define buf (make-buffer channels))
  (pa-start-stream stream)
  (bp stream buf))
(define (bytes-play! a-bp bs)
  (match-define (bp stream buf) a-bp)
  (with-handlers ([exn:fail?
                   (λ (x) (eprintf "~a\n" (exn-message x)))])
    (pa-write-stream stream bs samples-per-buffer)))
(define (close-bytes-player! a-bp)
  (match-define (bp stream buf) a-bp)
  (pa-close-stream stream))

(provide
 (contract-out
  [make-bytes-player (-> bp?)]
  [bytes-play! (-> bp? bytes? void?)]
  [close-bytes-player! (-> bp? void?)]))
