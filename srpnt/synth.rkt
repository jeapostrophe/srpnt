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
(define 11b-period/c
  (if AUTHENTIC?
      (nat-pow2/c 11)
      exact-nonnegative-integer?))
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

(struct synth:frame (p1 p2 t1 t2 n1 n2 n3 ld rd))

(struct synth (m p1-% p2-% t1-% t2-% n1-reg n1-% n2-reg n2-% n3-reg n3-%) #:mutable)
(define (make-synth m)
  (synth m 0.0 0.0 0.0 0.0 1 0.0 1 0.0 1 0.0))
(define (synth-step! s f)
  (define m (synth-m s))
  (match-define (synth:frame p1 p2 t1 t2 n1 n2 n3 ld rd) f)
  (match-define (wave:pulse p1-duty p1-period p1-vol)
    (or p1 off-wave:pulse))
  (match-define (wave:pulse p2-duty p2-period p2-vol)
    (or p2 off-wave:pulse))
  (match-define (wave:triangle t1-on? t1-period)
    (or t1 off-wave:triangle))
  (match-define (wave:triangle t2-on? t2-period)
    (or t2 off-wave:triangle))
  (match-define (wave:noise n1-short? n1-period n1-volume)
    (or n1 off-wave:noise))
  (match-define (wave:noise n2-short? n2-period n2-volume)
    (or n2 off-wave:noise))
  (match-define (wave:noise n3-short? n3-period n3-volume)
    (or n3 off-wave:noise))
  (match-define (wave:dmc ld-bs ld-off)
    (or ld off-wave:dmc))
  (match-define (wave:dmc rd-bs rd-off)
    (or rd off-wave:dmc))

  (mixer-begin! m)
  (for ([i (in-range samples-per-buffer)])
    (match-define (synth _ p1-% p2-% t1-% t2-% n1-reg n1-% n2-reg n2-% n3-reg n3-%) s)
    (define-values (p1 new-p1-%)
      (pulse-wave p1-duty p1-period p1-vol p1-%))
    (define-values (p2 new-p2-%)
      (pulse-wave p2-duty p2-period p2-vol p2-%))
    (define-values (t1 new-t1-%)
      (triangle-wave t1-on? t1-period t1-%))
    (define-values (t2 new-t2-%)
      (triangle-wave t2-on? t2-period t2-%))
    (define-values (n1 new-n1-reg new-n1-%)
      (noise n1-short? n1-period n1-volume n1-reg n1-%))
    (define-values (n2 new-n2-reg new-n2-%)
      (noise n2-short? n2-period n2-volume n2-reg n2-%))
    (define-values (n3 new-n3-reg new-n3-%)
      (noise n3-short? n3-period n3-volume n3-reg n3-%))
    (define ld
      (bytes-ref ld-bs (fx+ ld-off i)))
    (define rd
      (bytes-ref rd-bs (fx+ rd-off i)))

    (set-synth-p1-%! s new-p1-%)
    (set-synth-p2-%! s new-p2-%)
    (set-synth-t1-%! s new-t1-%)
    (set-synth-t2-%! s new-t2-%)
    (set-synth-n1-reg! s new-n1-reg)
    (set-synth-n1-%! s new-n1-%)
    (set-synth-n2-reg! s new-n2-reg)
    (set-synth-n2-%! s new-n2-%)
    (set-synth-n3-reg! s new-n3-reg)
    (set-synth-n3-%! s new-n3-%)
    (mixer-mix! m i p1 p2 t1 t2 n1 n2 n3 ld rd))
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
     [t1 (or/c #f wave:triangle?)]
     [t2 (or/c #f wave:triangle?)]
     [n1 (or/c #f wave:noise?)]
     [n2 (or/c #f wave:noise?)]
     [n3 (or/c #f wave:noise?)]
     [ld (or/c #f wave:dmc?)]
     [rd (or/c #f wave:dmc?)])]
  [synth? (-> any/c boolean?)]
  [make-synth (-> mixer? synth?)]
  [synth-step! (-> synth? synth:frame? any)]))
