#lang racket/base
(require srpnt/bytes-player
         racket/match
         racket/require
         (for-syntax racket/base)
         (filtered-in (λ (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops)
         racket/performance-hint)
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

(define (make-p-mix-bytes)
  (define v (make-bytes (* 16 16)))
  (for* ([p1 (in-range 16)]
         [p2 (in-range 16)])
    (bytes-set! v (p-mix-off p1 p2)
                (fl->fx (flround (fl* 127.0 (p-mix p1 p2))))))
  v)

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

;; period is unsigned 11-bits
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
;; period is unsigned 11-bits
(define (triangle-period->pitch period)
  (fl/ CPU-FREQ-Hz (fl* 32.0 (fl+ 1.0 (fx->fl period)))))
(define (triangle-pitch->period pitch)
  (fl->fx (flround (fl- (fl/ CPU-FREQ-Hz (fl* 32.0 pitch)) 1.0))))
;; xxx I think this is wrong
(define (triangle-wave on? period angle)
  (define pitch (triangle-period->pitch period))
  (define next-angle (angle-add/unit angle (fl* pitch inv-sample-rate.0)))
  (define angle-as-step (fl->fx (flfloor (fl* angle 31.0))))
  (define out
    (if on?
        (bytes-ref TRIANGLE-PATTERN angle-as-step)
        0))
  (values out next-angle))

(define (noise mode period register angle)
  #f)

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

(module+ main
  (require racket/math
           racket/gui/base
           racket/class)

  (define MONO-P1-bs (make-buffer 1))
  (define MONO-P2-bs (make-buffer 1))
  (define MONO-T-bs (make-buffer 1))
  (define MONO-N-bs (make-buffer 1))
  (define STEREO-D-bs (make-buffer channels))
  (define STEREO-COMBINED-bs (make-buffer channels))

  (define OSC-W samples-per-buffer)
  (define OSC-Hshort (fx* 2 16))
  (define OSC-Htall 128)
  (define OSC-MARGIN 8)
  (define OSCSshort (+ 1 1 1 1))
  (define OSCStall (+ 1 1 1 1))

  (define W
    (+ OSC-MARGIN OSC-W OSC-MARGIN))
  (define H
    (+ OSC-MARGIN
       (* (+ OSC-Hshort OSC-MARGIN) OSCSshort)
       (* (+ OSC-Htall OSC-MARGIN) OSCStall)
       OSC-MARGIN))
  (define (paint! c dc)
    (send dc set-background "black")
    (send dc set-text-foreground "yellow")
    (send dc set-pen "yellow" 1 'solid)
    (send dc clear)

    (begin-encourage-inline
     (define (draw-osc! label start-y bs channels off start-x)
       (send dc draw-text label OSC-MARGIN start-y)
       (for/fold ([last-x -1] [last-y 0])
                 ([i (in-range samples-per-buffer)])
         (define this-x i)
         (define this-y (fx- (bytes-ref bs (fx+ off (fx* i channels))) start-x))
         (send dc draw-line
               (fx+ OSC-MARGIN last-x) (fx- start-y last-y)
               (fx+ OSC-MARGIN this-x) (fx- start-y this-y))
         (values this-x this-y))))

    (define start-y 0)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Hshort)))
    (draw-osc! "Pulse-1" start-y MONO-P1-bs 1 0 0)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Hshort)))
    (draw-osc! "Pulse-2" start-y MONO-P2-bs 1 0 0)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Hshort)))
    (draw-osc! "Triangle" start-y MONO-T-bs 1 0 0)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Hshort)))
    (draw-osc! "Noise" start-y MONO-N-bs 1 0 0)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Htall)))
    (draw-osc! "DMC-L" start-y STEREO-D-bs channels 0 0)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Htall)))
    (draw-osc! "DMC-R" start-y STEREO-D-bs channels 1 0)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Htall)))
    (draw-osc! "Combined-L" start-y STEREO-COMBINED-bs channels 0 128)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Htall)))
    (draw-osc! "Combined-R" start-y STEREO-COMBINED-bs channels 1 128)
    (void))
  (define f (new frame% [label "SRPNT"] [width W] [height H]
                 [min-width W] [min-height H]))
  (define c (new canvas% [parent f] [paint-callback paint!]
                 [min-width W] [min-height H]))

  (when #f
    (thread
     (lambda ()
       (define frame-delay (fl* (fl/ 1.0 60.0) 1000.0))
       (let loop ()
         (define start (current-inexact-milliseconds))
         (send c refresh-now)
         (yield (alarm-evt (fl+ start frame-delay)))
         (loop))))
    (send f show #t))

  (define bp (make-bytes-player))
  (define sample-bs (read-sample/gzip 0 4 "clip.raw.gz"))

  (define p1-period
    (pulse-pitch->period 261.626))
  (printf "P1: ~a\n" p1-period)
  (define p2-period
    (pulse-pitch->period 440.00))
  (printf "P2: ~a\n" p2-period)
  (define t-period
    (or #f
        (triangle-pitch->period 440.00)
        (triangle-pitch->period 174.614)))
  (printf "T: ~a\n" t-period)

  ;; http://problemkaputt.de/everynes.htm

  (printf "starting...\n")
  (define p1-angle 0.0)
  (define p2-angle 0.0)
  (define t-angle 0.0)
  (let loop ([s 0])
    (for ([i (in-range samples-per-buffer)])
      (define-values (p1 new-p1-angle)
        (pulse-wave 2 p1-period
                    (if (fx= (fxmodulo s 4) 1) 7 0) p1-angle))
      (define-values (p2 new-p2-angle)
        (pulse-wave 2 p2-period
                    (if (fx< s 30) #;(fx= (fxmodulo s 4) 1) 7 0) p2-angle))
      (define-values (t new-t-angle)
        (triangle-wave (if (fx< 30 s) #;(fx= (fxmodulo s 2) 2) #t 0)
                       t-period
                       t-angle))
      ;; xxx 16 preset pitches
      ;; xxx 2 modes? (looped noise)
      ;; xxx volume?
      (define n
        ;; xxx different prng?
        (if (fx= (fxmodulo s 4) 0)
            (random 16)
            0))
      (define d-offset
        (fxmodulo (fx+ (fx* s samples-per-buffer) i)
                  (bytes-length sample-bs)))
      (define d
        (bytes-ref sample-bs d-offset))

      (define p-mixed
        (bytes-ref pmix-bs (p-mix-off p1 p2)))
      (define tnd-mixed
        (bytes-ref tndmix-bs (tnd-mix-off t n d)))
      (define mixed
        (fx+ p-mixed tnd-mixed))
      (define out
        (fx+ 128 mixed))

      (set! p1-angle new-p1-angle)
      (set! p2-angle new-p2-angle)
      (set! t-angle new-t-angle)
      (bytes-set! MONO-P1-bs i p1)
      (bytes-set! MONO-P2-bs i p2)
      (bytes-set! MONO-T-bs i t)
      (bytes-set! MONO-N-bs i n)
      (bytes-set! STEREO-D-bs (fx+ 0 (fx* i channels)) d)
      (bytes-set! STEREO-D-bs (fx+ 1 (fx* i channels)) d)
      (bytes-set! STEREO-COMBINED-bs (fx+ 0 (fx* i channels)) out)
      (bytes-set! STEREO-COMBINED-bs (fx+ 1 (fx* i channels)) out))
    (bytes-play! bp STEREO-COMBINED-bs)
    (loop (fxmodulo (fx+ 1 s) 60)))
  (close-bytes-player! bp)
  (printf "...stop.\n"))
