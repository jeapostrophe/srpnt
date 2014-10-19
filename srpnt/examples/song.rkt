#lang racket/base
(require srpnt/music
         racket/fixnum
         racket/runtime-path)

(define-runtime-path clip-path "clip.raw.gz")
(define sample-bs (read-sample/gzip 0 4 clip-path))

(define bad-dudes-ex
  (cmd:hold* 10
             (cons
              (cmd:hold*f 40
                          (λ (f)
                            (cmd:frame* (wave:pulse (modulo f 3)
                                                    (pulse-freq->period 261.626)
                                                    7)
                                        #f #f #f #f #f)))
              (cmd:hold* 20
                         (cmd:frame* #f #f #f #f #f #f)))))

(define alien3-example
  (cmd:hold* 10
             (cons
              (for/list ([vol (in-list (list 0 7 6 5 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0))]
                         [dperiod (in-list (list -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 4 4 -3 -4 -4 -3 -4 -4 3 4 4 3 4 4 -3 -4 -4 -3 -4 -4 3 4 4 -3 -4 -4 -3))]
                         [duty (in-list (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1  0 0 0 0 0 0 0 0 1))])
                (cmd:frame* (wave:pulse duty
                                        (+ (pulse-freq->period 261.626) dperiod)
                                        vol)
                            #f #f #f #f #f))
              (cmd:hold* 20
                         (cmd:frame* #f #f #f #f #f #f)))))

(define gremlins2-example
  (cmd:hold* 10
             (list*
              (cmd:frame* (wave:pulse 2 (pulse-freq->period 261.626) 12)
                          #f #f #f #f #f)
              (cmd:hold* 3
                         (cmd:frame* (wave:pulse 1 (pulse-freq->period 261.626) 12)
                                     #f #f #f #f #f))
              (cmd:hold* 4
                         (cmd:frame* (wave:pulse 1 (pulse-freq->period 261.626) 11)
                                     #f #f #f #f #f))
              (cmd:hold* 4
                         (cmd:frame* (wave:pulse 1 (pulse-freq->period 261.626) 1)
                                     #f #f #f #f #f))
              (cmd:hold* 4
                         (cmd:frame* (wave:pulse 1 (pulse-freq->period 261.626) 3)
                                     #f #f #f #f #f))
              (cmd:hold* 16
                         (cmd:frame* #f #f #f #f #f #f)))))

(define noise-test-suite
  (for*/list ([short? (in-list (list #f #t))]
              [noise-p (in-range 16)])
    (cmd:hold* 15
               (cmd:frame* #f #f #f
                           (wave:noise short? noise-p 4)
                           #f #f))))

(define triangle-test-suite
  (for/list ([stp
              (for/list ([st (in-range -48 +83)])
                (hash-ref TRIANGLE st #f))]
             #:when stp)
    (cmd:hold* 15
               (cmd:frame* #f #f (wave:triangle #t stp) #f #f #f))))

(define pulse-test-suite
  (for/list ([stp
              (for/list ([st (in-range -48 +83)])
                (hash-ref PULSE st #f))]
             #:when stp)
    (for/list ([duty (in-list (list 0 1 2))])
      (cmd:hold* 15
                 (cmd:frame* (wave:pulse duty stp 4)
                             #f #f #f #f #f)))))

(define initial-test
  (list*
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
               (λ (f)
                 (define s (fx* f samples-per-buffer))
                 (cmd:frame* #f #f #f #f
                             (wave:dmc sample-bs s)
                             #f)))))

(define main-track
  (cmd:repeat
   initial-test
   #;
   (cmd:frame* #f #f #f #f #f #f)))

(provide main-track)
