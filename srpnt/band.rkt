#lang racket/base
(require racket/match
         racket/list
         racket/fixnum
         srpnt/music-theory
         srpnt/tracker)

(define (force-lazy-scale/tones scale rest? tones)
  (for/list ([t*o (in-list tones)]
             [can-be-rest? (in-list (list #t #t #t #f))])
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
      (match-define (list* note (cons tones _) more) n)
      (list* note (force-lazy-scale/tones scale rest? tones) more))))

;; xxx new interface: submit composition, arrangement, and
;; effects. have the player store some state (like what part, what
;; measure, what frame it is on) that gets updated every frame. when
;; you change arrangement, you may change tempo, so you port the
;; state. every frame, evaluate the composition on the frame number to
;; get the instruments.

(define (compile-song c s)
  (match-define (vector ts ap pattern parts) c)
  (match-define (vector scale-root scale-kind tempo
                        pulse1 pulse2 triangle1 triangle2 drum
                        mhtb base-octave melody-up tenor-down bass-down
                        part->dm part->rest-randoms)
                s)
  (define scale (scale-kind scale-root))
  
  (chorded-song->commands
   #:me (cons 0.25 tempo)
   #:ts ts
   #:drum drum
   #:instruments
   (let ()
     (define instruments (vector pulse1 pulse2 triangle1 triangle2))
     (match-define (list melody-idx harmony-idx tenor-idx bass-idx) mhtb)
     (vector (cons (vector-ref instruments melody-idx)
                   (fx+ base-octave melody-up))
             (cons (vector-ref instruments harmony-idx)
                   base-octave)
             (cons (vector-ref instruments tenor-idx)
                   (fx- base-octave tenor-down))
             (cons (vector-ref instruments bass-idx)
                   (fxmax 0 (fx- (fx- base-octave tenor-down) bass-down)))))
   #:measures
   (let ()
     (define part->rest?ss (make-hash))
     (for ([(p ms) (in-hash parts)])
       ;; This is really ugly code!
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

;; xxx contract
(provide compile-song)
