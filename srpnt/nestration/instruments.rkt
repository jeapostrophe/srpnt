#lang racket/base
(require srpnt/nestration/instrument)

(define i:pulse:off
  (i:pulse/spec
   #:duty (spec:constant 0)
   #:period (spec:constant 0)
   #:volume (spec:constant 0)))

(define (i:pulse:basic duty)
  (i:pulse/spec
   #:duty (spec:constant duty)
   #:period (spec:constant 0)
   #:volume (spec:constant 7)))

;; xxx duty cycle modulation
;; xxx plucky volume
;; xxx vibrato
;; xxx tremelo

(define is:pulses
  (list (i:pulse:basic 0)
        (i:pulse:basic 1)
        (i:pulse:basic 2)))

(define i:triangle:off
  (i:triangle/spec
   #:on? #f
   #:period (spec:constant 0)))

(define i:triangle:basic
  (i:triangle/spec
   #:on? #t
   #:period (spec:constant 0)))

(define is:triangles
  (list i:triangle:basic))

;; Drum periods...
;; 3, 4, 8 sound good
;; 9 is crunchy
;; 7 and C are okay

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

(define i:drum:off
  (i:drum/spec #:mode (spec:constant #f)
               #:period (spec:constant #xF)
               #:volume (spec:constant 0)))

(define i:drums:off
  (i:drums (vector i:drum:off i:drum:off i:drum:off)))

(define i:drums:basic
  (i:drums (vector i:drum:hihat i:drum:bass i:drum:snare)))

(define is:drums
  (list i:drums:basic
        (i:drums (vector i:drum:hihat
                         i:drum:snare
                         i:drum:bass))
        (i:drums (vector i:drum:off
                         i:drum:snare
                         i:drum:bass))
        (i:drums (vector i:drum:off
                         i:drum:bass
                         i:drum:snare))
        (i:drums (vector i:drum:off
                         i:drum:off
                         i:drum:off))))

;; xxx get more from here: http://en.wikipedia.org/wiki/Drum_beat

;; xxx study the awesome beats of sin and punishment 2
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

(define beats:4/4
  (list
   beat:alternating-on
   beat:straight-rock
   beat:duple-triplets
   beat:double-time
   beat:blast-beat
   beat:funk-beat
   beat:heavy-metal))

(define beats:4/4-one-accent
  (list (list (cons 0.125 1) (cons 0.125 0)
              (cons 0.125 0) (cons 0.125 0)
              (cons 0.125 0) (cons 0.125 0)
              (cons 0.125 0) (cons 0.125 0))
        (list (cons 0.125 2) (cons 0.125 0)
              (cons 0.125 0) (cons 0.125 0)
              (cons 0.125 0) (cons 0.125 0)
              (cons 0.125 0) (cons 0.125 0))))

(define beats:3/4
  (list
   (list (cons 0.125 1) (cons 0.125 0)
         (cons 0.125 0) (cons 0.125 0)
         (cons 0.125 0) (cons 0.125 0))))

(provide (all-defined-out))
