#lang racket/base
(require racket/flonum
         racket/fixnum)
(module+ test
  (require racket/file))

;; apu : state, command -> state, samples
;; called once for 1/60th

;; command = pulse x pulse x triangle x noise

;; pulse = duty[0,3] x volume[0,15] x period[0,2048)
;; xxx implement sweep, envelope, length?

;; triangle = on[0,1] x period[0,2048)

;; noise = mode[0,1] x period-id[0,15] x volume[0,15]

;; samples: sequence of [0.0,1.0]
;; see http://wiki.nesdev.com/w/index.php/APU_Mixer for formula

;; Maybe I could represent the DMC as a sample set and use that for
;; sound effects. I can also give two of those for stereo!

;; Maybe it would be best to have a separate mix function that takes
;; the PPNT samples and the DMC samples and mixes them, so that I can
;; do stereo more efficiently.

(define (p-mix p1 p2)
  (fl/ 95.88
       (fl+ (fl/ 8128.0
                 (fx->fl (fx+ p1 p2)))
            100.0)))

(define (p-mix-off p1 p2)
  (fx+ (fx* 16 p1) p2))

(define (make-p-mix-flvec)
  (define v (make-flvector (* 16 16)))
  (for* ([p1 (in-range 16)]
         [p2 (in-range 16)])
    (flvector-set! v (p-mix-off p1 p2) (p-mix p1 p2)))
  v)

(define (make-p-mix-bytes)
  (define v (make-bytes (* 16 16)))
  (for* ([p1 (in-range 16)]
         [p2 (in-range 16)])
    (bytes-set! v (p-mix-off p1 p2)
                (fl->fx (flround (fl* 255.0 (p-mix p1 p2))))))
  v)

(module+ test
  (define pm-flvec
    (time (make-p-mix-flvec)))
  (define pm-bytes
    (time (make-p-mix-bytes)))
  (display-to-file pm-bytes "pm.bin" #:exists 'replace))

(define (tnd-mix t n d)
  (fl/ 159.79
       (fl+ (fl/ 1.0
                 (fl+ (fl/ (fx->fl t) 8227.0)
                      (fl+ (fl/ (fx->fl n) 12241.0)
                           (fl/ (fx->fl d) 22638.0))))
            100.0)))

(define (tn-mix-off t n)
  (fx+ (fx* (fx* 128 16) t) (fx* 128 n)))
(define (tnd-mix-off t n d)
  (fx+ (tn-mix-off t n) d))

(define (make-tnd-mix-flvec)
  (define v (make-flvector (* 16 16 128)))
  (for* ([t (in-range 16)]
         [n (in-range 16)]
         [d (in-range 128)])
    (flvector-set! v
                   (tnd-mix-off t n d)
                   (tnd-mix t n d)))
  v)

(define (make-tnd-mix-bytes)
  (define v (make-bytes (* 16 16 128)))
  (for* ([t (in-range 16)]
         [n (in-range 16)]
         [d (in-range 128)])
    (bytes-set! v
                (tnd-mix-off t n d)
                (fl->fx (flround (fl* 255.0 (tnd-mix t n d))))))
  v)

(module+ test
  (define tnd-flvec
    (time (make-tnd-mix-flvec)))
  (define tnd-bytes
    (time (make-tnd-mix-bytes)))
  (display-to-file tnd-bytes "tnd.bin" #:exists 'replace))

(module+ test
  (define (check-range BW)
    (define M (fx->fl (expt 2 BW)))
    (for* ([p1 (in-range 16)]
           [p2 (in-range 16)]
           [t (in-range 16)]
           [n (in-range 16)]
           [d (in-range 128)])
      (define pm (fl* M (flvector-ref pm-flvec (p-mix-off p1 p2))))
      (define pm_a (fl->fx (flround pm)))
      (define tm (fl* M (flvector-ref tnd-flvec (tnd-mix-off t n d))))
      (define tm_a (fl->fx (flround tm)))
      (define fl (fl+ pm tm))
      (define fx (fx+ pm_a tm_a))
      (define err (flabs (fl- fl (fx->fl fx))))
      (when (> err 1.0)
        (eprintf "~v x ~v -> ~v [~v]\n" p1 p2 pm pm_a)
        (eprintf "~v x ~v x ~v -> ~v [~v]\n" t n d tm tm_a)
        (eprintf "~v x ~v -> ~v [~v] -> ~v\n" pm tm fl fx err)
        (error 'check-range "Found a problem with bit-depth ~v" M)))
    (eprintf "No problem with bit-depth ~v\n" BW))
  (check-range 8)
  (check-range 16)
  (check-range 24)
  (check-range 32))

(define (mix p1 p2 t n d)
  (define p_o (p-mix p1 p2))
  (define tnd_o (tnd-mix t n d))
  (fl+ p_o tnd_o))
