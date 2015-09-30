#lang racket/base
(require racket/runtime-path
         racket/match
         racket/list
         racket/promise
         data/enumerate
         data/enumerate/lib
         srpnt/music-theory
         srpnt/nestration/instruments
         srpnt/synth
         srpnt/apu
         srpnt/bithoven
         srpnt/nestration)

(define (use-rand-bithoven)
  (use-bithoven #f #f))

(define (use-bithoven n c #:style [style style:all]
                      #:inform [inform! void])
  (define b/e (force p:bithoven/e))
  (define bn (if n (modulo n (enum-count b/e)) (random-index b/e)))
  (define b-output (from-nat b/e bn))
  (define comp (bithoven->composition b-output))
  (define n/e (make-nestration/e #:style style comp))
  (define cn (if c (modulo c (enum-count n/e)) (random-index n/e)))
  (define strat (from-nat n/e cn))
  (inform! (cons bn cn))
  (cons comp strat))

(define (basic-audio pulse1 pulse2 triangle1 triangle2 drums drum-measure
                     #:measures [measures #f])
  (define the-scale scale-diatonic-major)
  (define how-many-notes-in-scale (length (the-scale 'C)))
  (define how-many-notes-in-updown-scale (* 2 how-many-notes-in-scale))
  (define how-many-notes (lcm 4 how-many-notes-in-updown-scale))
  (define how-many-measures (or measures (quotient how-many-notes 4)))
  (cons (vector
         ts:4:4 '(#f #f #f #f)
         '(P)
         (hasheq 'P
                 (for/list ([mi (in-range how-many-measures)])
                   (for/list ([ni (in-range 4)])
                     (define i (+ (* mi 4) ni))
                     (define updown-off
                       (remainder i how-many-notes-in-updown-scale))
                     (define off
                       (if (< updown-off how-many-notes-in-scale)
                           updown-off
                           (- how-many-notes-in-scale
                              (- updown-off how-many-notes-in-scale))))
                     `(0.25
                       ((,off . 0) (,off . 0) (,off . 0) (,off . 0))
                       . #f)))))
        (vector 'C the-scale 120
                pulse1 pulse2 triangle1 triangle2 drums
                (list 0 1 2 3) 4 0 0 0
                (hasheq 'P drum-measure)
                (hasheq 'P (cons #f empty)))))

(define (test-pulse pulse)
  (basic-audio pulse  i:pulse:off i:triangle:off i:triangle:off i:drums:off beat:straight-rock))
(define (test-triangle triangle)
  (basic-audio i:pulse:off i:pulse:off triangle i:triangle:off  i:drums:off beat:straight-rock))
(define (test-drums drums)
  (basic-audio i:pulse:off i:pulse:off i:triangle:off i:triangle:off drums beat:straight-rock))
(define (test-drum-beat beat)
  (basic-audio i:pulse:off i:pulse:off i:triangle:off i:triangle:off i:drums:basic beat))

(define-runtime-path clip-path "clip.raw.gz")
(define sample-bs (read-sample/gzip 0 4 clip-path))

;; xxx test samples

;; 16947999029795477887876025486712914

(define audio
  (or (test-pulse (i:pulse:plucky 2))

      (use-bithoven
       #:style
       (struct-copy style style:classic
                    [tempo/e (fin/e 160)])
       #f #f)
      
      (test-drum-beat beat:heavy-metal)

      (test-pulse (i:pulse:plucky 2))
      (test-pulse (i:pulse:natural 2))
      (test-pulse (i:pulse:basic 2))
     
      (use-rand-bithoven)

      (test-drums 
       (i:drums (vector i:drum:hihat
                        i:drum:bass
                        i:drum:snare)))

      
      (test-triangle i:triangle:basic)
      
      
      
      (test-pulse (i:pulse:vibrato 5.0 2))
      (test-pulse (i:pulse:tremolo 120.0 2))
      
      
      
      (test-drums i:drums:basic)      
        
      (test-pulse (i:pulse:natural 2))
      (test-pulse (i:pulse:plucky 2))
      (test-pulse (i:pulse:vibrato 5.0 2))
      (test-pulse (i:pulse:tremolo 120.0 2))
      (test-triangle (i:triangle:vibrato 6.5)) 
      (test-triangle (i:triangle:tremoloish 120.0)) 
      (test-triangle i:triangle:plucky)
      (test-triangle i:triangle:basic)))

(provide audio)

(provide (all-defined-out))

(module+ main
  (require srpnt/player
           srpnt/band)
  (match-define (cons c n) audio)
  (play-one! (compile-song c n)))
