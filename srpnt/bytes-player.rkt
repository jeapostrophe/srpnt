#lang racket/base
(require portaudio/portaudio
         portaudio/devices
         racket/match
         racket/fixnum
         racket/flonum
         racket/contract/base)

(define channels 2)
(define reasonable-latency (fl/ 1.0 60.0))
(define sample-rate 44100)
(define sample-rate.0 (fx->fl sample-rate))
(define samples-per-buffer (fxquotient sample-rate 60))
(define frames-per-buffer (fx* channels samples-per-buffer))

(define (make-buffer channels)
  (make-bytes (fx* channels samples-per-buffer)))

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
  [channels fixnum?]
  [samples-per-buffer fixnum?]
  [frames-per-buffer fixnum?]
  [sample-rate.0 flonum?]
  [make-buffer (-> byte? bytes?)]
  [make-bytes-player (-> bp?)]
  [bytes-play! (-> bp? bytes? void?)]
  [close-bytes-player! (-> bp? void?)]))

(module+ test
  (require racket/math
           racket/flonum)
  (define v (make-buffer channels))
  (define bp (make-bytes-player))
  (printf "1-second tone at 440 Hz\n")
  (for ([s (in-range 60)])
    (for ([j (in-range samples-per-buffer)])
      (define i (fx+ (fx* samples-per-buffer s) j))
      (define sample
        (fxmax
         0
         (fx+ 128
              (fl->fx
               (flround
                (fl* 127.0
                     (fl* 0.2
                          (flsin (fl* (fl/ (fl* pi 2.0)
                                           sample-rate.0)
                                      (fl* (fx->fl i)
                                           440.0))))))))))
      (bytes-set! v (fx+ 0 (fx* j channels)) sample)
      (bytes-set! v (fx+ 1 (fx* j channels)) sample))
    (bytes-play! bp v))
  (close-bytes-player! bp)
  (printf "...stop.\n"))
