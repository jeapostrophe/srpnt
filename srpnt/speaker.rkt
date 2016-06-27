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

(define (speaker:file p)
  (define op (open-output-file p #:exists 'replace))
  (speaker
   (λ (out-bs)
     (display out-bs op))
   (λ ()
     (close-output-port op))))

(define (speaker:fork s1 s2)
  (speaker
   (λ (out-bs)
     (speaker-output! s1 out-bs)
     (speaker-output! s2 out-bs))
   (λ ()
     (speaker-close! s1)
     (speaker-close! s2))))

(provide
 (contract-out
  [speaker:fork (-> speaker? speaker? speaker?)]
  [speaker:file (-> path-string? speaker?)]
  [speaker:real (-> speaker?)]))
