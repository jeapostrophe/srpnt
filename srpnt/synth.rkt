#lang racket/base
(require srpnt/speaker
         srpnt/apu
         srpnt/mixer
         racket/match
         racket/flonum
         racket/fixnum
         racket/contract/base
         racket/performance-hint)

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

(define off-wave:pulse (wave:pulse 0 0 0))
(define off-wave:triangle (wave:triangle #f 0))
(define off-wave:noise (wave:noise #f 0 0))
(define blank-dmc-bs (make-buffer 1))
(define off-wave:dmc (wave:dmc blank-dmc-bs 0))

(struct synth:frame (p1 p2 t n ld rd))

(struct synth (m p1-% p2-% t-% n-reg n-%) #:mutable)
(define (make-synth m)
  (synth m 0.0 0.0 0.0 1 0.0))
(define (synth-step! s f)
  (define m (synth-m s))
  (match-define (synth:frame p1 p2 t n ld rd) f)
  (match-define (wave:pulse p1-duty p1-period p1-vol)
    (or p1 off-wave:pulse))
  (match-define (wave:pulse p2-duty p2-period p2-vol)
    (or p2 off-wave:pulse))
  (match-define (wave:triangle t-on? t-period)
    (or t off-wave:triangle))
  (match-define (wave:noise n-short? n-period n-volume)
    (or n off-wave:noise))
  (match-define (wave:dmc ld-bs ld-off)
    (or ld off-wave:dmc))
  (match-define (wave:dmc rd-bs rd-off)
    (or rd off-wave:dmc))

  (mixer-begin! m)
  (for ([i (in-range samples-per-buffer)])
    (match-define (synth _ p1-% p2-% t-% n-reg n-%) s)
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

(provide
 (contract-out
  [nat-pow2/c (-> exact-nonnegative-integer? contract?)]
  [duty-n/c contract?]
  [11b-period/c contract?]
  [volume/c contract?]
  [4b-period/c contract?]
  [struct wave:pulse
    ([duty-n duty-n/c]
     [11b-period 11b-period/c]
     [volume volume/c])]
  [struct wave:triangle
    ([on? boolean?]
     [11b-period 11b-period/c])]
  [struct wave:noise
    ([short? boolean?]
     [4b-period 4b-period/c]
     [volume volume/c])]
  [struct wave:dmc
    ;; bs + offset at least the size of 1 buffer
    ([bs bytes?]
     [offset exact-nonnegative-integer?])]
  [struct synth:frame
    ([p1 (or/c #f wave:pulse?)]
     [p2 (or/c #f wave:pulse?)]
     [t (or/c #f wave:triangle?)]
     [n (or/c #f wave:noise?)]
     [ld (or/c #f wave:dmc?)]
     [rd (or/c #f wave:dmc?)])]
  [synth? (-> any/c boolean?)]
  [make-synth (-> mixer? synth?)]
  [synth-step! (-> synth? synth:frame? any)]))
