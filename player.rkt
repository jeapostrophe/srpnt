#lang racket/base
(require srpnt/bytes-player
         srpnt/apu
         racket/match
         racket/flonum
         racket/fixnum
         racket/contract/base)

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

(struct cmd:frame (p1 p2 t n ld rd))
(define (cmd:frame* p1 p2 t n ld rd)
  (cmd:frame (or p1 off-wave:pulse)
             (or p2 off-wave:pulse)
             (or t off-wave:triangle)
             (or n off-wave:noise)
             (or ld off-wave:dmc)
             (or rd off-wave:dmc)))
(struct cmd:seqn (l))
(define (cmd:seqn* . l)
  (cmd:seqn l))
(struct cmd:repeat (c))
(define (cmd:hold* frames c)
  (cmd:seqn
   (for/list ([f (in-range frames)])
     c)))
(define (cmd:hold*f frames cf)
  (cmd:seqn
   (for/list ([f (in-range frames)])
     (cf (fx* f samples-per-buffer)))))

;; XXX separate bp/bs into a mixer
(struct player (log-p tmp-log-p bp bs p1-% p2-% t-% n-reg n-%) #:mutable)
(define (make-player log-p tmp-log-p)
  (define bp (make-bytes-player))
  (define bs (make-buffer channels))
  (player log-p tmp-log-p bp bs 0.0 0.0 0.0 1 0.0))
(define (close-player! p)
  (match-define (player log-p tmp-log-p bp bs p1-% p2-% t-% n-reg n-%) p)
  (close-bytes-player! bp))
(define (player-step! p p1 p2 t n ld rd)
  (match-define (player log-p tmp-log-p bp bs _ _ _ _ _) p)
  (match-define (wave:pulse p1-duty p1-period p1-vol) p1)
  (match-define (wave:pulse p2-duty p2-period p2-vol) p2)
  (match-define (wave:triangle t-on? t-period) t)
  (match-define (wave:noise n-short? n-period n-volume) n)
  (match-define (wave:dmc ld-bs ld-off) ld)
  (match-define (wave:dmc rd-bs rd-off) rd)

  (define log-op #f)
  (define (begin-mix!)
    (set! log-op
          (and tmp-log-p (open-output-file tmp-log-p #:exists 'replace)))
    (void))
  (define (mix! i p1 p2 t n ld rd)
    (define p-mixed
      (p-mix p1 p2))
    (define Ltnd-mixed
      (tnd-mix t n ld))
    (define Rtnd-mixed
      (tnd-mix t n rd))
    (define lout
      (fx+ 128 (fx+ p-mixed Ltnd-mixed)))
    (define rout
      (fx+ 128 (fx+ p-mixed Rtnd-mixed)))
    (when tmp-log-p
      (write-bytes (bytes p1 p2 t n ld rd lout rout) log-op))
    (bytes-set! bs (fx+ 0 (fx* i channels)) lout)
    (bytes-set! bs (fx+ 1 (fx* i channels)) rout)
    (void))
  (define (end-mix!)
    (bytes-play! bp bs)
    (when tmp-log-p
      (close-output-port log-op)
      (rename-file-or-directory tmp-log-p log-p #t))
    (void))

  (begin-mix!)
  (for ([i (in-range samples-per-buffer)])
    (match-define (player _ _ _ _ p1-% p2-% t-% n-reg n-%) p)
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

    (set-player-p1-%! p new-p1-%)
    (set-player-p2-%! p new-p2-%)
    (set-player-t-%! p new-t-%)
    (set-player-n-reg! p new-n-reg)
    (set-player-n-%! p new-n-%)
    (mix! i p1 p2 t n ld rd))
  (end-mix!))

(define (play! c
               #:log-p [log-p #f])
  (define tmp-log-p
    (and log-p (format "~a.tmp" log-p)))
  (printf "starting...\n")
  (define p (make-player log-p tmp-log-p))
  (let loop ([c c])
    (match c
      [(cmd:seqn l)
       (for-each loop l)]
      [(cmd:repeat c)
       (let repeat ()
         (loop c)
         (repeat))]
      [(cmd:frame p1 p2 t n ld rd)
       (player-step! p p1 p2 t n ld rd)]))
  (close-player! p)
  (printf "...stop.\n"))

(module+ main
  (require racket/math
           racket/cmdline)

  (define-values (PULSE TRIANGLE)
    (let ()
      (local-require racket/file
                     racket/string)
      (define PULSE (make-hasheq))
      (define TRIANGLE (make-hasheq))
      (for ([note-line (in-list (file->lines "keys.txt"))]
            [row (in-naturals)])
        (match-define (cons note-s freq+offs) (string-split note-line))
        (define note-l
          (for/list ([note (in-list (string-split note-s "/"))])
            (regexp-replace #rx"♭"
                            (regexp-replace #rx"♯" note "#")
                            "b")))
        (define periods
          (for/list ([fo (in-list freq+offs)]
                     [i (in-naturals)]
                     #:when (even? i))
            (define f (string->number fo))
            (cons (pulse-freq->period f)
                  (triangle-freq->period f))))
        (for ([p (in-list periods)]
              [octave (in-naturals)])
          (match-define (cons pul tri) p)
          (define semitone (+ (- (* octave 12) (* 4 12)) row))
          (define stn semitone)
          (when pul
            (hash-set! PULSE stn pul))
          (when tri
            (hash-set! TRIANGLE stn tri))
          (for ([n (in-list note-l)])
            (define no (string->symbol (format "~a~a" n octave)))
            (when pul
              (hash-set! PULSE no pul))
            (when tri
              (hash-set! TRIANGLE no tri)))))
      (values PULSE TRIANGLE)))

  (define sample-bs (read-sample/gzip 0 4 "clip.raw.gz"))

  (when #t
    (command-line
     #:program "apu"
     #:args (log-p)
     (play! #:log-p log-p
            (cmd:repeat
             (cmd:seqn*
              (cmd:seqn
               (for*/list ([short? (in-list (list #f #t))]
                           [noise-p (in-range 16)])
                 (cmd:hold* 15
                            (cmd:frame* #f #f #f
                                        (wave:noise short? noise-p 4)
                                        #f #f))))
              (cmd:seqn
               (for/list ([stp
                           (for/list ([st (in-range -48 +83)])
                             (hash-ref TRIANGLE st #f))]
                          #:when stp)
                 (cmd:hold* 15
                            (cmd:frame* #f #f (wave:triangle #t stp) #f #f #f))))
              (cmd:seqn*
               (cmd:hold* 30
                          (cmd:frame* (wave:pulse 0 (pulse-freq->period 261.626) 4)
                                      #f #f #f #f #f))
               (cmd:hold* 30
                          (cmd:frame* #f
                                      (wave:pulse 2 (pulse-freq->period 440.00) 4)
                                      #f #f #f #f))
               (cmd:hold* 15
                          (cmd:frame* #f #f
                                      (wave:triangle #t (triangle-freq->period 440.00))
                                      #f #f #f))
               (cmd:hold* 15
                          (cmd:frame* #f #f #f
                                      (wave:noise #f 0 4)
                                      #f #f))
               ;; XXX This is not a convenient interface, need a way to
               ;; fuse a DMC across the other frames
               (cmd:hold*f 55
                           (λ (s)
                             (cmd:frame* #f #f #f #f
                                         (wave:dmc sample-bs s)
                                         #f))))
              (cmd:seqn
               (for/list ([stp
                           (for/list ([st (in-range -48 +83)])
                             (hash-ref PULSE st #f))]
                          #:when stp)
                 (cmd:hold* 15
                            (cmd:frame* (wave:pulse 2 stp 4)
                                        #f #f #f #f #f))))))))))
