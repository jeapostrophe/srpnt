#lang racket/base
;; XXX copied from portaudio
(require ffi/unsafe
         (only-in '#%foreign ffi-callback)
         portaudio/portaudio
         portaudio/devices
         racket/bool
         racket/math)

(define channels 2)
(define reasonable-latency 0.1)

;; given an bytes, a starting frame, a stopping frame or
;; false, and a sample rate, play the sound.
(define (bytes-play bs start-frame pre-stop-frame sample-rate)
  (define total-frames (/ (bytes-length bs) channels))
  (define stop-frame (or pre-stop-frame
                         total-frames))
  (define sound-frames (- stop-frame start-frame))
  (pa-maybe-initialize)
  (define sr/i (exact->inexact sample-rate))
  (define device-number (find-output-device reasonable-latency))
  (define device-latency (device-low-output-latency device-number))
  (define output-stream-parameters
    (make-pa-stream-parameters
     device-number ;; device
     2             ;; channels
     '(paUInt8)    ;; sample format
     device-latency ;; latency
     #f))            ;; host-specific info
  (define stream
    (pa-open-stream
     #f            ;; input parameters
     output-stream-parameters
     sr/i
     0             ;; frames-per-buffer
     '()           ;; stream-flags
     #f
     #f))
  (pa-start-stream stream)
  (pa-write-stream stream bs total-frames)
  (pa-close-stream stream))

(module+ main
  (define v (make-bytes (* channels 44100)))
  (for ([i (in-range 44100)])
    (define sample
      (max 0 (inexact->exact (round (* 128 (* 0.2 (sin (* (/ 1 44100) pi 2 i 302))))))))
    (bytes-set! v (* i channels) sample)
    (bytes-set! v (add1 (* i channels)) sample))

  (printf "1-second tone at 302 Hz\n")
  (bytes-play v 0 #f 44100)
  (printf "...stop.\n"))
