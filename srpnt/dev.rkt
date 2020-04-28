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
  (printf "Composition number: ~v\n" bn)
  (define b-output (from-nat b/e bn))
  (define comp (bithoven->composition b-output))
  (define n/e (make-nestration/e #:style style comp))
  (define cn (if c (modulo c (enum-count n/e)) (random-index n/e)))
  (printf "Nestration number: ~v\n" cn)
  (define strat (from-nat n/e cn))
  (inform! (cons bn cn))
  (cons comp strat))

(define (basic-audio pulse1 pulse2 triangle1 triangle2 drums drum-measure
                     #:bpm [bpm 120]
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
                       (((,off . 0) (,off . 0) (,off . 0) (,off . 0)) . #f)
                       . #f)))))
        (vector 'C the-scale bpm
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

(define audio
  (or

   #;(use-rand-bithoven)

   (use-bithoven
    #:style
    (struct-copy style style:classic
                 [scales/e (fin/e scale-diatonic-major)]
                 [tempo/e (fin/e 180)])
    #f #f)
   
   #;(basic-audio i:pulse:off i:pulse:off i:triangle:plucky i:triangle:off i:drums:basic
                beat:straight-rock
                #:bpm 90
                #:measures 60)
   

   ;; examples/bithoven-melodic-minor160.bin
   ;; Composition number: 112689913599145029760399753369564077417508307352863072009096706091575107074490822974755157820327328
   ;; Nestration number: 1947063280882832403
   (use-bithoven
    #:style
    (struct-copy style style:classic
                 [scales/e (fin/e scale-melodic-minor)]
                 [drums/e (fin/e i:drums:off)]
                 [tempo/e (fin/e 160)])
    #f #f)

   ;; examples/bithoven-all.bin
   ;; Composition number: 105370697650089816574974105282759653499238341156204192774604369084467049004939613509001127484518264
   ;; Nestration number: 13606271740551781396585046
   (use-rand-bithoven)
   
   ;; examples/bithoven-natural-minor100.bin
   ;; Composition number: 151082340486227048364359446585186219936648607538270884717246070010340145820201505988133216403205486
   ;; Nestration number: 1076478016002637264
   (use-bithoven
    #:style
    (struct-copy style style:classic
                 [scales/e (fin/e scale-natural-minor)]
                 [tempo/e (fin/e 100)])
    #f #f)

   ;; examples/bithoven-harmonic-minor120.bin
   ;; Composition number: 49460740888706885790347319921703009205952968322810492028727289302293397538115189299366981890531865
   ;; Nestration number: 22580309652954949
   (use-bithoven
    #:style
    (struct-copy style style:classic
                 [scales/e (fin/e scale-harmonic-minor)]
                 [tempo/e (fin/e 120)])
    #f #f)
   
   ;; examples/bithoven-major180.bin
   ;; Composition number: 159526143626800468123169814346150250253428891440038894278294580069522599233708729162444490949628638
   ;; Nestration number: 3148729328697556916
   (use-bithoven
    #:style
    (struct-copy style style:classic
                 [scales/e (fin/e scale-diatonic-major)]
                 [tempo/e (fin/e 180)])
    #f #f)

   ;; examples/drums-heavy-metal.bin
   (test-drum-beat beat:heavy-metal)
   ;; examples/drums-double-time.bin
   (test-drum-beat beat:double-time)
   ;; examples/drums-straight-rock.bin
   (test-drum-beat beat:straight-rock)

   ;; examples/noise-snare.bin
   (test-drums
    (vector i:drum:off
            i:drum:off
            i:drum:snare))
   ;; examples/noise-bass.bin
   (test-drums
    (vector i:drum:off
            i:drum:bass
            i:drum:off))
   ;; examples/noise-hihat.bin
   (test-drums
    (vector i:drum:hihat
            i:drum:off
            i:drum:off))
   
   ;; examples/triangle-tremoloish.bin
   (test-triangle (i:triangle:tremoloish 120.0))
   ;; examples/triangle-vibrato.bin
   (test-triangle (i:triangle:vibrato 6.5))
   ;; examples/triangle-adsr-plucky.bin
   (test-triangle i:triangle:plucky)

   ;; examples/pulse-vibrato.bin
   (test-pulse (i:pulse:vibrato 5.0 2))
   ;; examples/pulse-tremolo.bin
   (test-pulse (i:pulse:tremolo 120.0 2))
   ;; examples/pulse-adsr-plucky.bin
   (test-pulse (i:pulse:plucky 2))
   ;; examples/pulse-adsr-natural.bin
   (test-pulse (i:pulse:natural 2))

   ;; examples/basic-noise-metal.bin
   (test-drums
    (vector i:drum:noise:metal
            i:drum:noise:metal
            i:drum:noise:metal))
   ;; examples/basic-noise-normal.bin
   (test-drums
    (vector i:drum:noise
            i:drum:noise
            i:drum:noise))

   ;; examples/basic-pulse-1.bin
   (test-pulse (i:pulse:basic 1))
   ;; examples/basic-pulse-2.bin
   (test-pulse (i:pulse:basic 2))
   ;; examples/basic-pulse-3.bin
   (test-pulse (i:pulse:basic 3))
   ;; examples/basic-pulse-4.bin
   (test-pulse (i:pulse:basic 4))
   
   ;; examples/basic-triangle.bin
   (test-triangle i:triangle:basic)

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (use-bithoven
    #:style
    #;style:sad
    #;(struct-copy style style:classic
                   [tempo/e (fin/e 160)])
    (struct-copy style style:classic
                 [scales/e (fin/e scale-diatonic-major
                                  scale-natural-minor
                                  scale-melodic-minor
                                  scale-harmonic-minor)]
                 [tempo/e (range/e 130 180)])

    #;(struct-copy style style:classic
                   [scales/e (fin/e scale-harmonic-minor)]
                   [tempo/e (fin/e 120)])
    #f #f)
   
   ))

(provide audio
         use-bithoven)

(provide (all-defined-out))

(module+ main
  (require srpnt/player
           srpnt/band)
  (match-define (cons c n) audio)
  (play-one! (compile-song c n)))
