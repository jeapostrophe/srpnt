#lang racket/base
(require racket/match
         racket/list
         racket/fixnum
         racket/flonum
         racket/math
         data/enumerate
         data/enumerate/lib
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
(define (force-lazy-scale/measures scale rest?ss ms)
  (for/list ([ns (in-list ms)]
             [rest?s (in-list rest?ss)])
    (for/list ([n (in-list ns)]
               [rest? (in-list rest?s)])
      (match-define (list* note tones more) n)
      (list* note (force-lazy-scale/tones scale rest? tones) more))))

(define tone-names/e
  (from-list/e tone-names))

(define scales/e
  (from-list/e scales))

(define tempo/e
  (range/e 40 500))

(define rest-n/e
  (fin/e #f 4 5 6 7 8 9 10 11 12 13 14 15 16))

(define pulse/e (from-list/e is:pulses))
(define triangle/e (from-list/e is:triangles))
(define drums/e (from-list/e is:drums))

(define mhb/e
  (permutations-of-n/e 3))

(define (drum-measure/e ts ap)
  (cond
   [(eq? ts ts:4:4)
    (match ap
      ['(#t #f #f #f)
       (from-list/e
        beats:4/4-one-accent)]
      [_
       (from-list/e
        beats:4/4)])]
   [(eq? ts ts:3:4)
    (from-list/e
     beats:3/4)]))

(define (nestration/e c)
  (match-define (vector ts ap pattern parts) c)
  (vec/e tone-names/e scales/e tempo/e rest-n/e
         pulse/e pulse/e triangle/e drums/e
         mhb/e
         (many/e (drum-measure/e ts ap)
                 (hash-count parts))))

(define (random-index/printing e)
  (define n (random-index e))
  (define k (size e))
  (local-require racket/format)
  (define ks (~a k))
  (define ns (~a #:min-width (string-length ks) #:align 'right n))
  (printf "Using n =\n\t~a of\n\t~a\n\n" ns ks)
  n)

(define (nestration c)
  (define n/e (nestration/e c))
  (from-nat n/e (random-index/printing n/e)))

;; xxx new interface: submit composition, arrangement, and
;; effects. have the player store some state (like what part, what
;; measure, what frame it is on) that gets updated every frame. when
;; you change arrangement, you may change tempo, so you port the
;; state. every frame, evaluate the composition on the frame number to
;; get the instruments.

(define (nes-harmonic c s)
  (match-define (vector ts ap pattern parts) c)
  (match-define (vector scale-root scale-kind tempo rest-n
                        pulse1 pulse2 triangle drum
                        mhb dms)
                s)
  (define scale (scale-kind scale-root))

  (let ()
    (local-require racket/pretty)
    (pretty-print s))

  ;; xxx choose this (and make sure every instrument can play the
  ;; notes) and choose the gaps between MHB
  (define base-octave 3)
  (define melody-up 1)
  (define bass-down 1)

  (printf "Tempo is ~v\n" tempo)
  (chorded-song->commands*
   #:me (cons 0.25 tempo)
   #:ts ts
   #:drum drum
   #:instruments
   (let ()
     (define instruments (vector pulse1 pulse2 triangle))
     (match-define (list melody-idx harmony-idx bass-idx) mhb)
     (vector (cons (vector-ref instruments melody-idx)
                   (fx+ base-octave melody-up))
             (cons (vector-ref instruments harmony-idx)
                   base-octave)
             (cons (vector-ref instruments bass-idx)
                   (fx- base-octave bass-down))))
   #:measures
   (let ()
     (define part->rest?ss (make-hash))
     (for ([(p ms) (in-hash parts)])
       (define rest?ss
         (map (λ (ns)
                (build-list (length ns) (λ (i) (and rest-n (zero? (random rest-n))))))
              ms))
       (printf "~v\n" rest?ss)
       (hash-set! part->rest?ss p
                  rest?ss))
     (append*
      (for/list ([p (in-list pattern)])
        (force-lazy-scale/measures
         scale
         (hash-ref part->rest?ss p)
         (hash-ref parts p)))))
   #:drum-measures
   (let ()
     (define part->dms (make-hash))
     (for ([(p ms) (in-hash parts)]
           [dm (in-list dms)])
       (hash-set! part->dms p
                  (for/list ([m (in-list ms)]) dm)))
     (append*
      (for/list ([p (in-list pattern)])
        (hash-ref part->dms p))))))

(provide nes-harmonic
         nestration)
