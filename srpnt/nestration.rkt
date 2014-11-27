#lang racket/base
(require racket/match
         racket/list
         racket/fixnum
         racket/flonum
         racket/math
         srpnt/music
         srpnt/music-theory
         srpnt/tracker
         srpnt/nestration/instrument
         srpnt/nestration/instruments)

(define (force-lazy-scale/tones scale rest? tones)
  (for/list ([t*o (in-list tones)]
             [can-be-rest? (in-list (list #t #t #f))])
    (if (and can-be-rest? rest?)
        #f
        (match t*o
          [#f
           #f]
          [(cons off t-oct)
           (match-define (cons tone s-oct) (list-ref/modify modify/octave scale off))
           (cons tone (fx+ t-oct s-oct))]))))
(define (force-lazy-scale/measures scale rest-n ms)
  (for/list ([ns (in-list ms)])
    (for/list ([n (in-list ns)])
      (match-define (list* note tones more) n)
      (define rest? (and rest-n (zero? (random rest-n))))
      (list* note (force-lazy-scale/tones scale rest? tones) more))))

(define (select-from-list l)
  (list-ref l (random (length l))))
(provide select-from-list)

(define (select-drum-measure ts ap)
  ;; xxx make this more robust
  (cond
   [(eq? ts ts:4:4)
    (match ap
      ['(#t #f #f #f)
       (select-from-list
        (list
         (list (cons 0.125 1) (cons 0.125 0)
               (cons 0.125 0) (cons 0.125 0)
               (cons 0.125 0) (cons 0.125 0)
               (cons 0.125 0) (cons 0.125 0))
         (list (cons 0.125 2) (cons 0.125 0)
               (cons 0.125 0) (cons 0.125 0)
               (cons 0.125 0) (cons 0.125 0)
               (cons 0.125 0) (cons 0.125 0))))]
      [_
       (select-from-list
        (list
         beat:alternating-on
         beat:straight-rock
         beat:duple-triplets
         beat:double-time
         beat:blast-beat
         beat:funk-beat
         beat:heavy-metal))])]
   [(eq? ts ts:3:4)
    (list (cons 0.125 1) (cons 0.125 0)
          (cons 0.125 0) (cons 0.125 0)
          (cons 0.125 0) (cons 0.125 0))]))

(define (nestration)
  #f)

;; xxx new interface: submit composition, arrangement, and
;; effects. have the player store some state (like what part, what
;; measure, what frame it is on) that gets updated every frame. when
;; you change arrangement, you may change tempo, so you port the
;; state. every frame, evaluate the composition on the frame number to
;; get the instruments.

(define (composition->track c a)
  (define (convert scale-kind rest-n tempo drums?)
    (define scale-root (select-from-list tone-names))
    (define scale (scale-kind scale-root))
    (convert-with scale rest-n tempo drums?))

  (let ()
    (local-require racket/pretty)
    (pretty-print c))

  (match-define (vector ts ap pattern parts) c)
  ;; xxx choose this (and make sure every instrument can play the notes)
  (define base-octave 3)

  (define (convert-with scale rest-n tempo drums?)
    (printf "Tempo is ~v\n" tempo)
    (chorded-song->commands*
     #:me
     (cons 0.25 tempo)
     #:ts ts
     #:drum
     ;; xxx generate this
     (cond
      [drums?
       (i:drums (vector i:drum:hihat
                        i:drum:bass
                        i:drum:snare))]
      [else
       (i:drums (vector i:drum:off i:drum:off i:drum:off))])
     #:instruments
     ;; xxx generate this
     (let ()
       (match-define
        (list melody harmony bass)
        ((if #t (λ (x) x) shuffle)
         (list (i:pulse/spec
                #:duty
                (or (spec:constant 2)
                    (spec:adsr 'sustain
                               0 (spec:constant 0)
                               0 (spec:constant 0)
                               1 (spec:modulate 440.0 2 1)
                               0 (spec:constant 0)))
                #:period
                (or (spec:constant 0)
                    (spec:adsr 'sustain
                               20 (spec:linear -5 5)
                               05 (spec:linear 5 0)
                               10 (spec:constant 0)
                               05 (spec:linear 0 -5))
                    (spec:adsr 'sustain
                               0 (spec:constant 0)
                               0 (spec:constant 0)
                               1 (spec:modulate 440.0 0 10)
                               0 (spec:constant 0)))
                #:volume
                (or (spec:adsr 'sustain
                               10 (spec:linear 10 10)
                               10 (spec:linear 8 8)
                               10 (spec:linear 6 6)
                               10 (spec:linear 0 0))
                    (spec:constant 6)
                    (spec:adsr 'sustain
                               05 (spec:linear 0 15)
                               05 (spec:linear 15 6)
                               10 (spec:modulate 440.0 6 4)
                               05 (spec:linear 6 0))
                    (spec:adsr 'sustain
                               05 (spec:linear 0 15)
                               05 (spec:linear 15 6)
                               10 (spec:linear 6 6)
                               10 (spec:linear 6 0))))
               (or
                (i:pulse/spec
                 #:duty
                 (or (spec:constant 2)
                     (spec:adsr 'sustain
                                0 (spec:constant 0)
                                0 (spec:constant 0)
                                1 (spec:modulate 880.0 2 1)
                                0 (spec:constant 0)))
                 #:period (spec:constant 0)
                 #:volume (spec:constant 6))
                (i:pulse/spec
                 #:duty (spec:constant 2)
                 #:period
                 (spec:adsr 'sustain
                            0 (spec:constant #f)
                            0 (spec:constant #f)
                            1 (spec:modulate 440.0 0 2)
                            0 (spec:constant #f))
                 #:volume (spec:adsr 'sustain
                                     4 (spec:constant 15)
                                     9 (spec:linear 15 6)
                                     0 (spec:constant 0)
                                     0 (spec:constant 0))))
               (i:triangle/spec
                #:period
                (or (spec:adsr 'sustain
                               0 (spec:constant 0)
                               0 (spec:constant 0)
                               1 (spec:modulate 880.0 0 5)
                               0 (spec:constant 0))
                    (spec:constant 0))))))
       (vector (cons melody (fx+ base-octave 1))
               (cons harmony base-octave)
               (cons bass (fx- base-octave 1))))
     #:measures
     (append*
      (for/list ([p (in-list pattern)])
        (force-lazy-scale/measures scale rest-n (hash-ref parts p))))
     #:drum-measures
     (let ()
       (define part->dms (make-hash))
       (append*
        (for/list ([p (in-list pattern)])
          (hash-ref! part->dms p
                     (λ ()
                       (define dm (select-drum-measure ts ap))
                       (define ms (hash-ref parts p))
                       (for/list ([m (in-list ms)]) dm))))))))

  (when #f
    (list
     (convert scale-diatonic-major 8 480 #f)
     (convert scale-diatonic-major 8 280 #t)
     (convert scale-diminished #f 90 #t)
     (convert scale-harmonic-minor 16 210 #t)
     (convert (select-from-list scales)
              (and (zero? (random 2)) (+ 4 (random 8)))
              (+ 50 (random 400))
              #t)))

  (convert scale-diatonic-major 8 140 #t))

(provide composition->track
         nestration)
