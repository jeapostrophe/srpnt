#lang racket/base
(require racket/match
         racket/list
         racket/contract
         racket/fixnum
         racket/flonum
         racket/math
         data/enumerate
         data/enumerate/lib
         srpnt/enum-util
         srpnt/tones
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
  (apply fin/e tone-names))

(define scales/e (apply fin/e scales))
(define tempo/e (range/e 40 500))

(define rest-n/e
  (fin/e #f 4 5 6 7 8 9 10 11 12 13 14 15 16))

(define pulse/e (apply fin/e is:pulses))
(define triangle/e (apply fin/e is:triangles))
(define drums/e (apply fin/e is:drums))

(define mhb/e
  (permutations-of-n/e 3))

(define (drum-measure/e ts ap)
  (cond
   [(eq? ts ts:4:4)
    (match ap
      ['(#t #f #f #f)
       (apply fin/e
              beats:4/4-one-accent)]
      [_
       (apply fin/e
              beats:4/4)])
    ;; xxx this is more interesting, but "wrong"
    (apply fin/e
           beats:4/4)]
   [(eq? ts ts:3:4)
    (apply fin/e
           beats:3/4)]))

(struct style
  (name tone-names/e scales/e tempo/e pulse1/e pulse2/e triangle/e drums/e mhb/e))

(define-syntax-rule (define-styles styles [id . expr] ...)
  (begin (define id (style . expr)) ...
         (define styles (list id ...))
         (provide style-name styles id ...)))

(define-styles styles
  [style:classic
   "Classic"
   tone-names/e (fin/e scale-diatonic-major) (range/e 160 300)
   (apply fin/e is:pulses-classic)
   (apply fin/e is:pulses-classic)
   (fin/e i:triangle:basic i:triangle:plucky)
   (fin/e i:drums:basic)
   (fin/e (list 0 1 2))]
  [style:happy
   "Happy"
   tone-names/e (fin/e scale-diatonic-major) (fin/e 200)
   pulse/e pulse/e triangle/e drums/e mhb/e]
  [style:sad
   "Sad"
   tone-names/e (fin/e scale-harmonic-minor) (fin/e 120)
   pulse/e pulse/e triangle/e drums/e mhb/e]
  [style:all
   "ALL"
   tone-names/e scales/e tempo/e pulse/e pulse/e triangle/e drums/e mhb/e])

(define (nestration/e
         #:style [style style:all]
         #:tone-names/e [tone-names/e (style-tone-names/e style)]
         #:scales/e [scales/e (style-scales/e style)]
         #:tempo/e [tempo/e (style-tempo/e style)]
         #:pulse1/e [pulse1/e (style-pulse1/e style)]
         #:pulse2/e [pulse2/e (style-pulse2/e style)]
         #:triangle/e [triangle/e (style-triangle/e style)]
         #:drums/e [drums/e (style-drums/e style)]
         #:mhb/e [mhb/e (style-mhb/e style)]
         c)
  (match-define (vector ts ap pattern parts) c)
  (vector/e tone-names/e scales/e tempo/e
            pulse1/e pulse2/e triangle/e drums/e
            mhb/e
            ;; xxx these should be dependent on the instruments and how
            ;; they will be used, because the pulse can't go very low and
            ;; the triangle can't go very high
            (fin/e 2 3) (fin/e 1 2) (fin/e 1 2)
            (hash-traverse/e
             #:get-contract (λ (x) (listof (cons/c real? exact-nonnegative-integer?)))
             (λ (_) (drum-measure/e ts ap))
             parts)
            (hash-traverse/e
             #:get-contract (λ (x) (listof exact-nonnegative-integer?))
             (λ (ms)
               (dep/e rest-n/e
                      #:f-range-finite? #t
                      (λ (rest-n)
                        (if rest-n
                            (listof-n/e
                             (below/e rest-n)
                             (add1 (ceiling (/ (length (append* ms)) rest-n))))
                            (single/e '())))))
             parts)))

(define (nestration+idx c #:n/e [n/e (nestration/e c)])
  (define n (random-index/printing n/e))
  (values n (from-nat n/e n)))

(define (nestration c #:n/e [n/e (nestration/e c)])
  (define-values (a b) (nestration+idx c #:n/e n/e))
  b)

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
         nestration
         nestration+idx
         nestration/e)
