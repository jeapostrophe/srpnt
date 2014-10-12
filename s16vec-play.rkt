#lang racket/base
;; XXX copied from portaudio
(require ffi/vector
         ffi/unsafe
         (only-in '#%foreign ffi-callback)
         portaudio/portaudio
         portaudio/callback-support
         portaudio/devices
         racket/bool
         racket/math)

(define channels 2)
(define reasonable-latency 0.1)

;; given an s16vec, a starting frame, a stopping frame or
;; false, and a sample rate, play the sound.
(define (s16vec-play s16vec start-frame pre-stop-frame sample-rate)
  (define total-frames (/ (s16vector-length s16vec) channels))
  (define stop-frame (or pre-stop-frame
                         total-frames))
  (define sound-frames (- stop-frame start-frame))
  (pa-maybe-initialize)
  (define copying-info (make-copying-info s16vec start-frame stop-frame))
  (define sr/i (exact->inexact sample-rate))
  (define device-number (find-output-device reasonable-latency))
  (define device-latency (device-low-output-latency device-number))
  (define output-stream-parameters
    (make-pa-stream-parameters
     device-number ;; device
     2             ;; channels
     '(paInt16)    ;; sample format
     device-latency ;; latency
     #f))            ;; host-specific info
  (define stream
    (pa-open-stream
     #f            ;; input parameters
     output-stream-parameters
     sr/i
     0             ;; frames-per-buffer
     '()           ;; stream-flags
     copying-callback
     copying-info))
  (pa-set-stream-finished-callback
   stream
   copying-info-free)
  (pa-start-stream stream)
  (define (stopper)
    (pa-close-stream stream)
    (void))

  ;; this is the "worse is better" solution to closing streams;
  ;; polling is bad, but callbacks from C into Racket seem really
  ;; fragile, and the polling here can afford to be quite coarse.
  ;; the danger of not polling enough is that too many streams
  ;; will be open, and that the system will start rejecting open-stream
  ;; calls. As of 2013, Ubuntu seems to support 32 streams, and OS X
  ;; an unbounded number. What about Windows? Dunno, let's go check.
  (define sound-seconds (/ sound-frames sample-rate))
  (define expected-startup-latency 0.02)
  (define fail-wait 0.5)
  (thread
   (lambda ()
     ;; hopefully this is long enough for the sound to finish:
     (sleep (+ expected-startup-latency sound-seconds))
     (let loop ()
       (cond [(stream-already-closed? stream)
              ;; nothing to be done
              #f]
             [(not (pa-stream-active? stream))
              ;; inactive, close it:
              (pa-close-stream stream)]
             [else
              ;; wait and try again:
              (begin (sleep fail-wait)
                     (loop))]))))
  stopper)

(let ()
  (define v (make-s16vector (* channels 44100)))
  (for ([i (in-range 10000)])
    (define sample (inexact->exact (round (* 32767 (* 0.2 (sin (* (/ 1 44100) pi 2 i 302)))))))
    (s16vector-set! v (* i channels) sample)
    (s16vector-set! v (add1 (* i channels)) sample))
  (for ([i (in-range 10000 44100)])
    (define sample (inexact->exact (round (* 32767 (* 0.2 (sin (* (/ 1 44100) pi 2 i 500)))))))
    (s16vector-set! v (* i channels) sample)
    (s16vector-set! v (add1 (* i channels)) sample))

  (printf "1/2-second tone at 302/500 Hz\n")
  (sleep 2)
  (s16vec-play v 0 #f 44100)
  (sleep 1)
  (printf "...stop.\n"))
