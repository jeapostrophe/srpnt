#lang racket/base
(require srpnt/speaker
         srpnt/apu
         srpnt/mixer
         racket/match
         racket/flonum
         racket/fixnum
         racket/contract/base
         racket/performance-hint)

;; Synth

(define (nat-pow2/c k)
  (and/c fixnum? (between/c 0 (fx- (expt 2 k) 1))))
(define duty-n/c (nat-pow2/c 2))
(define 11b-period/c (nat-pow2/c 11))
(define volume/c (nat-pow2/c 4))
(define 4b-period/c (nat-pow2/c 4))

(struct wave:pulse (duty-n 11b-period volume))
(struct wave:triangle (on? 11b-period))
(struct wave:noise (short? 4b-period volume))
(struct wave:dmc (bs offset))

(struct synth (p1-% p2-% t-% n-reg n-%) #:mutable)
(define (make-synth)
  (synth 0.0 0.0 0.0 1 0.0))
(define (synth-step! m s p1 p2 t n ld rd)
  (match-define (wave:pulse p1-duty p1-period p1-vol) p1)
  (match-define (wave:pulse p2-duty p2-period p2-vol) p2)
  (match-define (wave:triangle t-on? t-period) t)
  (match-define (wave:noise n-short? n-period n-volume) n)
  (match-define (wave:dmc ld-bs ld-off) ld)
  (match-define (wave:dmc rd-bs rd-off) rd)

  (mixer-begin! m)
  (for ([i (in-range samples-per-buffer)])
    (match-define (synth p1-% p2-% t-% n-reg n-%) s)
    (define-values (p1 new-p1-%)
      (pulse-wave p1-duty p1-period p1-vol p1-%))
    (define-values (p2 new-p2-%)
      (pulse-wave p2-duty p2-period p2-vol p2-%))
    (define-values (t new-t-%)
      (triangle-wave t-on? t-period t-%))
    (define-values (n new-n-reg new-n-%)
      (noise n-short? n-period n-volume n-reg n-%))
    (define ld
      (bytes-ref ld-bs (fx+ ld-off i)))
    (define rd
      (bytes-ref rd-bs (fx+ rd-off i)))

    (set-synth-p1-%! s new-p1-%)
    (set-synth-p2-%! s new-p2-%)
    (set-synth-t-%! s new-t-%)
    (set-synth-n-reg! s new-n-reg)
    (set-synth-n-%! s new-n-%)
    (mixer-mix! m i p1 p2 t n ld rd))
  (mixer-end! m))

;; Helpers

(define off-wave:pulse (wave:pulse 0 0 0))
(define off-wave:triangle (wave:triangle #f 0))
(define off-wave:noise (wave:noise #f 0 0))
(define blank-dmc-bs (make-buffer 1))
(define off-wave:dmc (wave:dmc blank-dmc-bs 0))

;; A frame corresponds to 1/60th of a second.
(struct cmd:frame (p1 p2 t n ld rd))

(provide (struct-out wave:pulse)
         off-wave:pulse
         (struct-out wave:triangle)
         off-wave:triangle
         (struct-out wave:noise)
         off-wave:noise
         (struct-out wave:dmc)
         off-wave:dmc
         (struct-out cmd:frame))

(define (play-to! m init-c)
  (define s (make-synth))
  (let loop ([c init-c])
    (match c
      [(or #f '() (? void?))
       (void)]
      [(cons a d)
       (loop a)
       (loop d)]
      [(cmd:frame p1 p2 t n ld rd)
       (synth-step! m s p1 p2 t n ld rd)])))

(define (playing-mixer)
  (mixer:standard (speaker:real)))

(define (play-one! init-c)
  (define m (playing-mixer))
  (play-to! m init-c)
  (mixer-close! m))

(provide play-one!)
