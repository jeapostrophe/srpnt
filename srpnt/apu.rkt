#lang racket/base
(require (only-in srpnt/speaker sample-rate.0)
         racket/flonum
         racket/fixnum
         racket/performance-hint)

(define AUTHENTIC? #f)

;; The NES APU is synchronized with the clock of the CPU and all of
;; its sounds are specified by period (i.e. iterations of the CPU per
;; cycle) rather than frequency (cycles per second). We could do a
;; clock-accurate simulation and implement periods directly, but I
;; don't think it is worth it, so instead we convert the period into
;; the frequency.
(define CPU-FREQ-MHz 1.789773)
(define CPU-FREQ-Hz (fl* CPU-FREQ-MHz (fl* 1000.0 1000.0)))

(define max-pulse-period (fx- (expt 2 11) 1))
(begin-encourage-inline
  (define (pulse-period->freq period)
    (fl/ CPU-FREQ-Hz (fl* 16.0 (fl+ 1.0 (fx->fl period))))))
(define (pulse-freq->period freq)
  (define pre-period (fl/ CPU-FREQ-Hz (fl* 16.0 freq)))
  (define r (fl->fx (flround (fl- pre-period 1.0))))
  (if AUTHENTIC?
      (if (and (fx<= 0 r) (fx<= r max-pulse-period))
          r
          #f)
      r))

;; The triangle wave actually runs twice as slow which makes its
;; sounds lower and makes it so a period twice as long gets the same
;; sound as the pulse.
(begin-encourage-inline
  (define (triangle-period->freq period)
    (fl/ (pulse-period->freq period) 2.0)))
(define (triangle-freq->period freq)
  (pulse-freq->period (fl* 2.0 freq)))

;; In the case of triangle and pulse, I am slightly inaccurate by
;; allowing frequencies where the corresponding period is specially
;; coded to be ignored.

;; In the code below, we implements periods/frequencies as a % of a
;; single cycle, i.e. a value between [0,1]. We turn a frequency
;; (cycles per second) into a %-increment (cycles per sample) by
;; dividing by the sample-rate. Every time a sample is generated, we
;; increment and then remove any values that go above 1.0
(begin-encourage-inline
  (define (cycle%-step % freq)
    (define %step (fl/ freq sample-rate.0))
    (define next% (fl+ % %step))
    (fl- next% (flfloor next%))))

;; The pulse wave has only 4 duty cycles. A duty cycle is the
;; percentage of a cycle that the sound is active. Since we represent
;; the cycle has a percentage, this is very easy to implement.
(define DUTY-CYCLES (flvector 0.125 0.25 0.50 0.75))
(begin-encourage-inline
  (define (duty-n->cycle n)
    (flvector-ref DUTY-CYCLES n)))

(begin-encourage-inline
  (define (pulse-wave duty-n period volume %)
    (define freq (pulse-period->freq period))
    (define duty-cycle (duty-n->cycle duty-n))
    (define next-% (cycle%-step % freq))
    (define out
      (if (fl< next-% duty-cycle)
          volume
          0))
    (values out next-%)))

;; The triangle goes through a fixed amplitude pattern (it has no
;; volumen control). We implement this by multiplying the cycle
;; percentage by the width of the pattern, thus getting the sample of
;; the pattern to look at.
(define TRIANGLE-PATTERN
  (bytes 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
         0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15))
(begin-encourage-inline
  (define (triangle-wave on? period %)
    (define freq (triangle-period->freq period))
    (define next-% (cycle%-step % freq))
    (define %-as-step (fl->fx (flround (fl* next-% 31.0))))
    (define out
      (if on?
          (bytes-ref TRIANGLE-PATTERN %-as-step)
          0))
    (values out next-%)))

;; The noise channel is a 15-bit linear feedback shift register that
;; is initialized to 1. Every cycle the PRNG steps. The period is a
;; 4-bit selector from a specific period table. Then, the noise runs
;; much faster than the other oscillators (8 times!). Next, the noise
;; has two modes that correspond to the length of the PRNG cycle.

(define NOISE-PERIODS
  (vector 4 8 16 32 64 96 128 160 202 254 380 508 762 1016 2034 4068))
(begin-encourage-inline
  (define (noise-period->freq period)
    (fl* (pulse-period->freq period) 8.0)))

(begin-encourage-inline
  (define (noise short? period volume register %)
    (define freq (noise-period->freq period))
    (define next-% (cycle%-step % freq))
    (define next-register
      (cond
        ;; A cycle has ended when the next-% is smaller than the current one
        [(fl< next-% %)
         (define (bit i) (bitwise-bit-field register i (fx+ i 1)))
         (define other-bit (if short? 6 1))
         (define feedback (bitwise-xor (bit 0) (bit other-bit)))
         (define shifted-ref (arithmetic-shift register -1))
         (define feedback-at-bit14 (arithmetic-shift feedback 14))
         (bitwise-ior shifted-ref feedback-at-bit14)]
        [else
         register]))

    (values
     ;; The volume is emitted when the register's 0 bit is 1
     (fx* volume (fxmodulo next-register 2))
     next-register
     next-%)))

;; The NES DMC could play samples encoding in a complicated
;; way. Ultimately a 7-bit sound was produced. I just represent the
;; samples directly as 7-bit PCM data.

(define (read-sample/port base mult p)
  (define-values (7bit-out 7bit-in) (make-pipe))
  (local-require racket/port)
  (let loop ()
    (define c (read-byte p))
    (cond
      [(eof-object? c)
       (void)]
      [else
       (define b (fxmin (fxmax 0 (fx+ base (fx* mult (fx- c 128)))) 127))
       (write-byte b 7bit-in)
       (loop)]))
  (close-output-port 7bit-in)
  (port->bytes 7bit-out))
(define (read-sample/path base mult p)
  (read-sample/port base mult (open-input-file p)))
(define (read-sample/gzip base mult p)
  (local-require file/gunzip)
  (define-values (ungzipped gzipped) (make-pipe))
  (gunzip-through-ports (open-input-file p) gzipped)
  (close-output-port gzipped)
  (read-sample/port base mult ungzipped))

;; The NES uses a strange non-linear mixing scheme. Each of the waves
;; produces a 4-bit value except for the DMC which produces a 7-bit
;; value. These are combined in analog in the NES, which would imply
;; using a very accurate (float) representation. However, I stick them
;; all into 7-bit audio after doing an accurate mixing. Furthermore, I
;; "optimize" this by storing it pre-computed in a bytes-array

(define (raw-p-mix p1 p2)
  (fl/ 95.88
       (fl+ (fl/ 8128.0
                 (fx->fl (fx+ p1 p2)))
            100.0)))

(begin-encourage-inline
  (define (p-mix-off p1 p2)
    (fx+ (fx* 16 p1) p2)))

(define (make-p-mix-bytes)
  (define v (make-bytes (* 16 16)))
  (for* ([p1 (in-range 16)]
         [p2 (in-range 16)])
    (bytes-set! v (p-mix-off p1 p2)
                (fl->fx (flround (fl* 127.0 (raw-p-mix p1 p2))))))
  v)

(define p-mix-bs
  (make-p-mix-bytes))

(begin-encourage-inline
  (define (p-mix p1 p2)
    (bytes-ref p-mix-bs (p-mix-off p1 p2))))

(define (raw-tnd-mix t n d)
  (fl/ 159.79
       (fl+ (fl/ 1.0
                 (fl+ (fl/ (fx->fl t) 8227.0)
                      (fl+ (fl/ (fx->fl n) 12241.0)
                           (fl/ (fx->fl d) 22638.0))))
            100.0)))

(begin-encourage-inline
  (define (tn-mix-off t n)
    (fx+ (fx* (fx* 128 16) t) (fx* 128 n)))
  (define (tnd-mix-off t n d)
    (fx+ (tn-mix-off t n) d)))

(define (make-tnd-mix-bytes)
  (define v (make-bytes (* 16 16 128)))
  (for* ([t (in-range 16)]
         [n (in-range 16)]
         [d (in-range 128)])
    (bytes-set! v
                (tnd-mix-off t n d)
                (fl->fx (flround (fl* 127.0 (raw-tnd-mix t n d))))))
  v)

(define tnd-mix-bs
  (make-tnd-mix-bytes))

(begin-encourage-inline
  (define (tnd-mix t n d)
    (bytes-ref tnd-mix-bs (tnd-mix-off t n d))))

(provide AUTHENTIC?
         pulse-wave
         triangle-wave
         noise
         p-mix
         tnd-mix
         pulse-freq->period
         triangle-freq->period
         read-sample/port
         read-sample/path
         read-sample/gzip)
