#lang racket/base
(require racket/match
         racket/flonum
         racket/fixnum
         racket/contract/base
         racket/performance-hint)

(define channels 2)
(define sample-rate 44100)
(define sample-rate.0 (fx->fl sample-rate))
(define samples-per-buffer (fxquotient sample-rate 60))
(define frames-per-buffer (fx* channels samples-per-buffer))

(define (make-buffer channels)
  (make-bytes (fx* channels samples-per-buffer)))

(struct speaker (output close))
(begin-encourage-inline
  (define (speaker-output! sp v)
    ((speaker-output sp) v))
  (define (speaker-close! sp)
    ((speaker-close sp))))

(provide
 (struct-out speaker)
 speaker-output!
 speaker-close!
 
 (contract-out
  [channels fixnum?]
  [samples-per-buffer fixnum?]
  [frames-per-buffer fixnum?]
  [sample-rate.0 flonum?]
  [make-buffer (-> byte? bytes?)]))
