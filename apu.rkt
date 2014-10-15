#lang racket/base
(require srpnt/bytes-player
         racket/match
         racket/flonum
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
                (fl->fx (flround (fl* 127.0 (p-mix p1 p2))))))
  v)

(module+ test
  (define pm-flvec
    (time (make-p-mix-flvec)))
  (define pm-bytes
    (time (make-p-mix-bytes)))
  (display-to-file pm-bytes "pm.bin" #:exists 'replace))

(define pmix-bs
  (make-p-mix-bytes))

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
                (fl->fx (flround (fl* 127.0 (tnd-mix t n d))))))
  v)

(define tndmix-bs
  (make-tnd-mix-bytes))

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
    (eprintf "No problem with bit-depth ~v: ~v\n" BW (expt 2 BW)))
  (check-range 7))

(define (mix p1 p2 t n d)
  (define p_o (p-mix p1 p2))
  (define tnd_o (tnd-mix t n d))
  (fl+ p_o tnd_o))

;; http://opengameart.org/forumtopic/kickin-it-old-school-setting-up-nes-style-chiptunes

;; Look at: http://www.mattmontag.com/projects-page/nintendo-vst
(define inv-sample-rate.0
  (fl/ 1.0 sample-rate.0))

(define (angle-add/unit a b)
  (define sum (fl+ a b))
  (cond [(fl<= 1.0 sum)
         (fl- sum 1.0)]
        [else
         sum]))

(define (duty-n->cycle n)
  (match n
    [0 0.125]
    [1 0.25]
    [2 0.50]
    [3 0.75]))

(define CPU-FREQ-Hz (fl* 1.789773 (fl* 1000.0 1000.0)))

(define (pulse-period->pitch period)
  (fl/ CPU-FREQ-Hz (fl* 16.0 (fl+ 1.0 (fx->fl period)))))
(define (pulse-pitch->period pitch)
  (define pre-period (fl/ CPU-FREQ-Hz (fl* 16.0 pitch)))
  (define r (fl->fx (flround (fl- pre-period 1.0))))
  r)
(define (pulse-wave duty-n period volume angle)
  (define pitch (pulse-period->pitch period))
  (define duty-cycle (duty-n->cycle duty-n))
  (define next-angle (angle-add/unit angle (fl* pitch inv-sample-rate.0)))
  (define out
    (if (fl< angle duty-cycle)
        volume
        0))
  (values out next-angle))

(define TRIANGLE-PATTERN
  (bytes 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
         0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15))
(define (triangle-period->pitch period)
  (fl/ CPU-FREQ-Hz (fl* 32.0 (fl+ 1.0 (fx->fl period)))))
(define (triangle-pitch->period pitch)
  (fl->fx (flround (fl- (fl/ CPU-FREQ-Hz (fl* 32.0 pitch)) 1.0))))
(define (triangle-wave on? period step angle)
  (define pitch (triangle-period->pitch period))
  (define next-angle (angle-add/unit angle (fl* pitch inv-sample-rate.0)))
  (define out
    (if (and on? (fl< angle 0.5))
        (bytes-ref TRIANGLE-PATTERN step)
        0))
  (values out (fxmodulo (fx+ 1 step) 32) next-angle))

(define (read-sample/port base mult p)
  (define-values (7bit-out 7bit-in) (make-pipe))
  (local-require racket/port)
  (let loop ()
    (match (read-byte p)
      [(? eof-object?)
       (void)]
      [c
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

(define (display-sample-rle bs)
  (define-values (last count m M)
    (for/fold ([last 0] [count 0] [m 127] [M 0])
              ([b (in-bytes bs)])
      (cond
       [(fx= last b)
        (values last (fx+ count 1) m M)]
       [else
        (printf "~a[~a] " last count)
        (values b 1 (fxmin m b) (fxmax M b))])))
  (printf "\n~v samples in [~v,~v]\n" (bytes-length bs) m M)
  (void))

(module+ main
  (require racket/math
           racket/flonum)
  (define v (make-buffer channels))
  (define bp (make-bytes-player))
  (define sample-bs (read-sample/gzip 0 4 "clip.raw.gz"))
  (display-sample-rle sample-bs)

  (printf "starting...\n")
  (define p1-angle 0.0)
  (define p2-angle 0.0)
  (define t-step 0)
  (define t-angle 0.0)
  (for ([s (in-range 60)])
    (bytes-fill! v 128)
    (for ([i (in-range frames-per-buffer)])
      (define-values (p1 new-p1-angle)
        (pulse-wave 2 (pulse-pitch->period 261.626)
                    (if (fx= (fxmodulo s 4) 0) 7 0) p1-angle))
      (define-values (p2 new-p2-angle)
        (pulse-wave 0 (pulse-pitch->period 440.000)
                    (if (fx= (fxmodulo s 4) 1) 7 0) p2-angle))
      (define-values (t new-t-step new-t-angle)
        (triangle-wave (fx= (fxmodulo s 4) 2) (triangle-pitch->period 174.614)
                       t-step t-angle))
      ;; xxx 16 preset pitches
      ;; xxx 2 modes? (looped noise)
      (define n
        ;; xxx different prng?
        (if (fx= (fxmodulo s 4) 3)
            (random 16)
            0))
      (define d-offset (fx+ (fx* s frames-per-buffer) i))
      (define d
        (if (fx< d-offset (bytes-length sample-bs))
            (bytes-ref sample-bs d-offset)
            0))

      (define p-mixed
        (bytes-ref pmix-bs (p-mix-off p1 p2)))
      (define tnd-mixed
        (bytes-ref tndmix-bs (tnd-mix-off t n d)))
      (define mixed
        (fx+ p-mixed tnd-mixed))
      (define out
        (+ 128 mixed))

      (set! p1-angle new-p1-angle)
      (set! p2-angle new-p2-angle)
      (set! t-step new-t-step)
      (set! t-angle new-t-angle)
      (bytes-set! v (fx+ 0 (fx* i channels)) out)
      (bytes-set! v (fx+ 1 (fx* i channels)) out))
    (bytes-play! bp v))
  (close-bytes-player! bp)
  (printf "...stop.\n"))
