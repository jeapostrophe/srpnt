#lang racket/base
(require racket/fixnum
         srpnt/nestration/instrument)

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

(define (i:pulse:modulating freq)
  (i:pulse/spec
   #:duty (spec:% (spec:modulate freq 1 1))
   #:period (spec:constant 0)
   #:volume (spec:constant 7)))

(define (i:pulse:plucky duty)
  (i:pulse/spec
   #:duty (spec:constant duty)
   #:period (spec:constant 0)
   #:volume
   (spec:adsr 'release
              4 (spec:constant 14)
              4 (spec:linear 14 7)
              4 (spec:constant 7)
              4 (spec:linear 7 0))))

(define (i:pulse:natural duty)
  (i:pulse/spec
   #:duty (spec:constant duty)
   #:period (spec:constant 0)
   #:volume
   (spec:adsr 'sustain
              4 (spec:constant 14)
              4 (spec:linear 14 7)
              4 (spec:constant 7)
              4 (spec:linear 7 0))))

(define (i:pulse:vibrato freq duty)
  (i:pulse/spec
   #:duty (spec:constant duty)
   #:period (spec:% (spec:modulate freq 0 5))
   #:volume (spec:constant 7)))

(define (i:pulse:tremolo freq duty)
  (i:pulse/spec
   #:duty (spec:constant duty)
   #:period (spec:constant 0)
   #:volume (spec:% (spec:modulate freq 7 4))))

(define duties '(0 1 2))
(define is:pulses-classic
  (append
   (map i:pulse:basic duties)
   (map i:pulse:plucky duties)
   (map i:pulse:natural duties)))
(define is:pulses
  (append
   is:pulses-classic
   (list (i:pulse:modulating 60.0)
         (i:pulse:modulating 140.0))
   (for*/list ([duty (in-list duties)]
               [freq (in-list (list 5.0 6.5))])
     (i:pulse:vibrato freq duty))
   (for*/list ([duty (in-list duties)]
               [freq (in-list (list 10.0 60.0 120.0))])
     (i:pulse:tremolo freq duty))))

(define i:triangle:off
  (i:triangle/spec
   #:on? (spec:constant #f)
   #:period (spec:constant 0)))

(define i:triangle:basic
  (i:triangle/spec
   #:on? (spec:constant #t)
   #:period (spec:constant 0)))

(define (i:triangle:vibrato freq)
  (i:triangle/spec
   #:on? (spec:constant #t)
   #:period (spec:% (spec:modulate freq 0 5))))

(define (i:triangle:tremoloish freq)
  (i:triangle/spec
   #:on? (spec:%
          (spec:bind (spec:modulate freq 0 1)
                     (λ (v) (fx<= 0 v))))
   #:period (spec:constant 0)))

(define i:triangle:plucky
  (i:triangle/spec
   #:on?
   (spec:adsr 'release
              4 (spec:constant #t)
              4 (spec:constant #t)
              4 (spec:constant #t)
              8 (spec:constant #f))
   #:period (spec:constant 0)))

(define is:triangles
  (list i:triangle:basic
        (i:triangle:vibrato 5.0)
        (i:triangle:vibrato 6.5)
        i:triangle:plucky))

;; Drum periods...
;; 3, 4, 8 sound good
;; 9 is crunchy
;; 7 and C are okay

(define hihat-adsr
  (spec:adsr 'release
             1 (spec:constant 4)
             2 (spec:constant 3)
             4 (spec:constant 2)
             4 (spec:constant 0)))
(define i:drum:hihat
  (i:drum/spec #:mode (spec:constant #f)
               #:period (spec:constant #xC)
               #:volume hihat-adsr))
(define i:drum:hihat:metal
  (i:drum/spec #:mode (spec:constant #t)
               #:period (spec:constant #xC)
               #:volume hihat-adsr))

(define i:drum:bass
  (i:drum/spec #:mode (spec:constant #f)
               #:period (spec:constant 9)
               #:volume
               (spec:adsr 'release
                          1 (spec:constant 10)
                          2 (spec:constant 7)
                          4 (spec:linear 4 2)
                          4 (spec:constant 0))))

(define snare-adsr
  (spec:adsr 'release
             1 (spec:constant 11)
             4 (spec:linear 11 6)
             8 (spec:linear 6 2)
             4 (spec:constant 0)))
(define i:drum:snare
  (i:drum/spec #:mode (spec:constant #f)
               #:period (spec:constant 7)
               #:volume snare-adsr))
(define i:drum:snare:metal
  (i:drum/spec #:mode (spec:constant #t)
               #:period (spec:constant 7)
               #:volume snare-adsr))

(define i:drum:off
  (i:drum/spec #:mode (spec:constant #f)
               #:period (spec:constant #xF)
               #:volume (spec:constant 0)))

(define is:drum
  (list i:drum:off
        i:drum:hihat
        i:drum:hihat:metal
        i:drum:snare
        i:drum:snare:metal
        i:drum:bass))

(define i:drums:off
  (i:drums (vector i:drum:off i:drum:off i:drum:off)))

(define i:drums:basic
  (i:drums (vector i:drum:hihat i:drum:bass i:drum:snare)))

(define is:drums
  (for*/list ([hihat (in-list is:drum)]
              [bass (in-list is:drum)]
              [snare (in-list is:drum)]
              #:unless (and (eq? hihat bass)
                            (eq? bass snare)))
    (i:drums (vector hihat bass snare))))

;; From: http://en.wikipedia.org/wiki/Drum_beat

;; MORE study the awesome beats of sin and punishment 2
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

(provide i:drums
         (all-defined-out))
