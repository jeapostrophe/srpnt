#lang racket/base
(require srpnt/speaker
         srpnt/apu
         racket/fixnum
         racket/contract/base
         racket/performance-hint)

(struct mixer (begin mix end close))
(begin-encourage-inline
  (define (mixer-begin! m) ((mixer-begin m)))
  (define (mixer-mix! m i p1 p2 t1 t2 n1 n2 n3 ld rd)
    ((mixer-mix m) i p1 p2 t1 t2 n1 n2 n3 ld rd))
  (define (mixer-end! m) ((mixer-end m)))
  (define (mixer-close! m) ((mixer-close m))))

(define (mixer:standard sp)
  (define out-ch channels)
  (define out-bs (make-buffer out-ch))
  (define begin! void)
  (define (mix! i p1 p2 t1 t2 n1 n2 n3 ld rd)
    (define base-mixed
      (fx+ (fx+ n1 (fx+ n2 n3))
           (fx+ (fx+ p1 p2)
                (fx+ t1 t2))))
    #; #; #;
    (define p-mixed (p-mix p1 p2))
    (define Ltnd-mixed (tnd-mix t n ld))
    (define Rtnd-mixed (tnd-mix t n rd))
    ;; xxx this is too big, because ld/rd can be up to 127
    (define lout
      (fx+ 128 (fx+ base-mixed ld)))
    (define rout
      (fx+ 128 (fx+ base-mixed rd)))
    (bytes-set! out-bs (fx+ 0 (fx* i out-ch)) lout)
    (bytes-set! out-bs (fx+ 1 (fx* i out-ch)) rout)
    (void))
  (define (end!)
    (speaker-output! sp out-bs)
    (void))
  (define (close!)
    (speaker-close! sp))
  (mixer begin! mix! end! close!))

(provide
 mixer-begin! mixer-mix! mixer-end! mixer-close!
 (contract-out
  [mixer? (-> any/c boolean?)]
  [mixer:standard (-> speaker? mixer?)]))
