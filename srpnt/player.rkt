#lang racket/base
(require srpnt/speaker
         srpnt/apu
         racket/match
         racket/flonum
         racket/fixnum
         racket/contract/base
         racket/performance-hint)

;; Mixer API

(struct mixer (begin mix end close))
(begin-encourage-inline
  (define (mixer-begin! m) ((mixer-begin m)))
  (define (mixer-mix! m i p1 p2 t n ld rd) ((mixer-mix m) i p1 p2 t n ld rd))
  (define (mixer-end! m) ((mixer-end m)))
  (define (mixer-close! m) ((mixer-close m))))

;; Standard Mixer

(define (mixer:standard sp)
  (define out-ch channels)
  (define out-bs (make-buffer out-ch))
  (define begin! void)
  (define (mix! i p1 p2 t n ld rd)
    (define p-mixed (p-mix p1 p2))
    (define Ltnd-mixed (tnd-mix t n ld))
    (define Rtnd-mixed (tnd-mix t n rd))
    (define lout
      (fx+ 128 (fx+ p-mixed Ltnd-mixed)))
    (define rout
      (fx+ 128 (fx+ p-mixed Rtnd-mixed)))
    (bytes-set! out-bs (fx+ 0 (fx* i out-ch)) lout)
    (bytes-set! out-bs (fx+ 1 (fx* i out-ch)) rout)
    (void))
  (define (end!)
    (speaker-output! sp out-bs)
    (void))
  (define (close!)
    (speaker-close! sp))
  (mixer begin! mix! end! close!))

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
(struct cmd:repeat (c))

(provide (struct-out wave:pulse)
         off-wave:pulse
         (struct-out wave:triangle)
         off-wave:triangle
         (struct-out wave:noise)
         off-wave:noise
         (struct-out wave:dmc)
         off-wave:dmc
         (struct-out cmd:frame)
         (struct-out cmd:repeat))

(define (play-to! m init-c)
  (define (play-cmd init-c)
    (define s (make-synth))
    (let loop ([c init-c])
      (match c
        [(or #f '() (? void?))
         (void)]
        [(cons a d)
         (loop a)
         (loop d)]
        [(cmd:repeat c)
         (let repeat ()
           (loop c)
           (repeat))]
        [(cmd:frame p1 p2 t n ld rd)
         (synth-step! m s p1 p2 t n ld rd)])))
  (play-cmd init-c))

(define (playing-mixer)
  (mixer:standard (speaker:real)))

(define (play-one! init-c)
  (define m (playing-mixer))
  (play-to! m init-c)
  (mixer-close! m))

(provide play-one!)

(define (dynamic-play-to! m song-p)
  (define ns (make-base-namespace))
  (namespace-attach-module (current-namespace) 'srpnt/player ns)
  (define init-c
    (parameterize ([current-namespace ns])
      (dynamic-require `(file ,song-p) 'main-track)))
  (play-to! m init-c))

(define (play! song-p)
  (printf "starting...\n")
  (define m
    (playing-mixer))
  
  (let loop ()
    (printf "Loading ~a\n" song-p)
    (define player-t
      (thread
       (λ ()
         (dynamic-play-to! m song-p))))
    (define song-p-evt
      (filesystem-change-evt song-p))
    (sync song-p-evt player-t)
    (filesystem-change-evt-cancel song-p-evt)
    (kill-thread player-t)
    (loop))

  (mixer-close! m)
  (printf "...stop.\n"))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "player"
   #:args (song-file)
   (play!
    song-file)))
