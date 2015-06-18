#lang racket/base
(require racket/runtime-path
         racket/match
         racket/list
         data/enumerate
         data/enumerate/lib
         srpnt/music-theory
         srpnt/nestration/instruments
         srpnt/synth
         srpnt/apu
         srpnt/bithoven
         srpnt/nestration)

(define (use-bithoven)
  (define b-output (from-nat bithoven/e (random-index bithoven/e)))
  (define comp (bithoven->composition b-output))
  (define n/e (make-nestration/e comp))
  (define strat (from-nat n/e (random-index n/e)))
  (cons comp strat))

(define (basic-audio pulse1 pulse2 triangle drums drum-measure)
  (define the-scale scale-diatonic-major)
  (define how-many-notes-in-scale (length (the-scale 'C)))
  (define how-many-notes-in-updown-scale (* 2 how-many-notes-in-scale))
  (define how-many-notes (lcm 4 how-many-notes-in-updown-scale))
  (define how-many-measures (quotient how-many-notes 4))
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
                       ((,off . 0) (,off . 0) (,off . 0))
                       . #f)))))
        (vector 'C the-scale 120
                pulse1 pulse2 triangle drums
                (list 0 1 2) 4 0 0
                (hasheq 'P drum-measure)
                (hasheq 'P
                        (cons #f empty)))))

(define (test-pulse pulse)
  (basic-audio i:pulse:off pulse i:triangle:off i:drums:off beat:straight-rock))
(define (test-triangle triangle)
  (basic-audio i:pulse:off i:pulse:off triangle i:drums:off beat:straight-rock))
(define (test-drums drums)
  (basic-audio i:pulse:off i:pulse:off i:triangle:off drums beat:straight-rock))
(define (test-drum-beat beat)
  (basic-audio i:pulse:off i:pulse:off i:triangle:off i:drums:basic beat))

(define-runtime-path clip-path "clip.raw.gz")
(define sample-bs (read-sample/gzip 0 4 clip-path))

;; xxx test samples

(define audio
  (or (use-bithoven)

      (test-drum-beat beat:funk-beat)

      (test-drums 
       (i:drums (vector i:drum:hihat
                        i:drum:bass
                        i:drum:snare)))

      
      (test-triangle i:triangle:basic)
      (test-pulse (i:pulse:natural 2))
      (test-pulse (i:pulse:plucky 2))
      (test-pulse (i:pulse:basic 2))
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

(module+ main
  (require srpnt/player
           srpnt/band)
  (match-define (cons c n) audio)
  (play-one! (compile-song c n)))
