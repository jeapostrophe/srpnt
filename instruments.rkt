#lang racket/base
(require srpnt/nestration/instrument)

;; xxx look up better formulas?
(define i:drum:hihat
  (i:drum/spec #:mode (spec:constant #f)
               #:period (spec:constant #xC)
               #:volume
               (spec:adsr 'release
                          1 (spec:constant 4)
                          2 (spec:constant 3)
                          4 (spec:constant 2)
                          4 (spec:constant 0))))

(define i:drum:bass
  (i:drum/spec #:mode (spec:constant #f)
               #:period (spec:constant 9)
               #:volume
               (spec:adsr 'release
                          1 (spec:constant 10)
                          2 (spec:constant 7)
                          4 (spec:linear 4 2)
                          4 (spec:constant 0))))

(define i:drum:snare
  (i:drum/spec #:mode (spec:constant #f)
               #:period (spec:constant 7)
               #:volume
               (spec:adsr 'release
                          1 (spec:constant 11)
                          4 (spec:linear 11 6)
                          8 (spec:linear 6 2)
                          4 (spec:constant 0))))

;; xxx get more from here: http://en.wikipedia.org/wiki/Drum_beat
(define beat:heavy-metal
  (list (cons 0.125 1) (cons 0.0625 1) (cons 0.0625 1)
        (cons 0.125 2) (cons 0.0625 1) (cons 0.0625 1)
        (cons 0.125 1) (cons 0.0625 1) (cons 0.0625 1)
        (cons 0.125 2) (cons 0.0625 1) (cons 0.0625 1)))
(define beat:blast-beat
  (list (cons 0.125 1) (cons 0.125 2)
        (cons 0.125 1) (cons 0.125 2)
        (cons 0.125 1) (cons 0.125 2)
        (cons 0.125 1) (cons 0.125 2)))
(define beat:funk-beat
  (list (cons 0.125 1) (cons 0.125 0)
        (cons 0.125 2) (cons 0.125 0)
        (cons 0.125 1) (cons 0.125 1)
        (cons 0.125 0) (cons 0.125 2)))
(define beat:double-time
  (list (cons 0.0625 1) (cons 0.0625 0) (cons 0.0625 2) (cons 0.0625 0)
        (cons 0.0625 1) (cons 0.0625 0) (cons 0.0625 2) (cons 0.0625 0)
        (cons 0.0625 1) (cons 0.0625 0) (cons 0.0625 2) (cons 0.0625 0)
        (cons 0.0625 1) (cons 0.0625 0) (cons 0.0625 2) (cons 0.0625 0)))
(define beat:straight-rock
  (list (cons 0.125 1) (cons 0.125 0)
        (cons 0.125 2) (cons 0.125 0)
        (cons 0.125 1) (cons 0.125 0)
        (cons 0.125 2) (cons 0.125 0)))
(define beat:alternating-on
  (list (cons 0.125 0) (cons 0.125 0)
        (cons 0.125 1) (cons 0.125 0)
        (cons 0.125 0) (cons 0.125 0)
        (cons 0.125 2) (cons 0.125 0)))
(define beat:duple-triplets
  (list (cons 0.125 1) (cons 0.0625 0) (cons 0.0625 0)
        (cons 0.125 2) (cons 0.0625 0) (cons 0.0625 0)
        (cons 0.125 1) (cons 0.0625 0) (cons 0.0625 0)
        (cons 0.125 2) (cons 0.0625 0) (cons 0.0625 0)))

(provide (all-defined-out))
