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
  (vec/e tone-names/e scales/e tempo/e
         pulse/e pulse/e triangle/e drums/e
         mhb/e
         ;; xxx these should be dependent on the instruments and how
         ;; they will be used, because the pulse can't go very low and
         ;; the triangle can't go very high
         (fin/e 3 4) (fin/e 1 2) (fin/e 1 2)
         (hash-traverse/e
          (λ (_) (drum-measure/e ts ap))
          parts)
         (hash-traverse/e
          (λ (ms)
            (dep/e rest-n/e
                   (λ (rest-n)
                     (if rest-n
                         (many/e
                          (below/e rest-n)
                          (add1 (ceiling (/ (length (append* ms)) rest-n))))
                         (const/e '())))))
          parts)))

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
  (match-define (vector scale-root scale-kind tempo
                        pulse1 pulse2 triangle drum
                        mhb base-octave melody-up bass-down
                        part->dm part->rest-randoms)
                s)
  (define scale (scale-kind scale-root))

  (let ()
    (local-require racket/pretty)
    (pretty-print s))

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
       ;; xxx this is really ugly code
       (match-define (cons rest-n rest-randoms)
                     (hash-ref part->rest-randoms p))
       (define (random!)
         (begin0 (first rest-randoms)
           (set! rest-randoms (rest rest-randoms))))
       (define next-rest (if rest-n (random!) +inf.0))
       (define i (if rest-n rest-n +inf.0))
       (define rest?ss
         (map (λ (ns)
                (build-list
                 (length ns)
                 (λ (j)
                   (begin0 (if (= next-rest 0)
                               #t
                               #f)
                     (cond
                      [(= i 1)
                       (set! i rest-n)
                       (set! next-rest (random!))]
                      [else
                       (set! i (- i 1))
                       (set! next-rest (- next-rest 1))])))))
              ms))
       (hash-set! part->rest?ss p rest?ss))
     (append*
      (for/list ([p (in-list pattern)])
        (force-lazy-scale/measures
         scale
         (hash-ref part->rest?ss p)
         (hash-ref parts p)))))
   #:drum-measures
   (let ()
     (define part->dms (make-hash))
     (for ([(p ms) (in-hash parts)])
       (define dm (hash-ref part->dm p))
       (hash-set! part->dms p
                  (for/list ([m (in-list ms)]) dm)))
     (append*
      (for/list ([p (in-list pattern)])
        (hash-ref part->dms p))))))

(provide nes-harmonic
         nestration)