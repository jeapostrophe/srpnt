#lang racket/base
(require racket/list
         racket/fixnum
         racket/flonum
         racket/match
         srpnt/music-theory
         math/base
         data/enumerate)
(module+ test
  (require rackunit))

(define-syntax-rule (λ/memo (arg ...) . body)
  (let ()
    (define MEMO (make-hash))
    (define (f arg ...)
      (define k (list* arg ...))
      (hash-ref! MEMO k
                 (λ () (let () . body))))
    f))
(define-syntax-rule (define/memo (f arg ...) . body)
  (define f (λ/memo (arg ...) . body)))

(define (select-from-list l)
  (list-ref l (random (length l))))
(provide select-from-list)

(struct time-sig (name ts) #:transparent)
(define time-sig/ts:4:4 (time-sig "4/4" ts:4:4))
(define time-sig/ts:3:4 (time-sig "3/4" ts:3:4))
(define time-sig/e
  (fin/e time-sig/ts:4:4))

;; xxx #t means "accented" and "can be the start of a chord
;; change", but the second thing isn't correctly used
(struct accent-pattern (name pulses-per-measure accents) #:transparent)

(define time-sig->accents
  (hash
   time-sig/ts:4:4
   (list (accent-pattern "standard"  1 '(#t #f #f #f))
         (accent-pattern "on-beats"  2 '(#t #f #t #f))
         (accent-pattern "off-beats" 2 '(#f #t #f #t)))
   time-sig/ts:3:4
   (list (accent-pattern "waltz"  1 '(#t #f #f)))))

(define (accent-pattern/e ts)
  (from-list/e (hash-ref time-sig->accents ts)))

(define (accent-pattern-notes-per-pulse ap)
  (match-define (accent-pattern _ ppm as) ap)
  (/ (length as) ppm))

;; xxx https://en.wikipedia.org/wiki/Musical_form#Levels_of_organization

;; part-lens is in 4 measures
(struct form (name part-lens pattern) #:transparent)
(define form/e
  (fin/e
   (form "strophic"
         '((A . 1))
         '(A))
   (form "medley"
         '((A . 1) (B . 1) (C . 1) (D . 1))
         '(A B C D))
   (form "double medley"
         '((A . 1) (B . 1) (C . 1) (D . 1))
         '(A A B B C C D D))
   (form "binary"
         '((A . 1) (B . 1))
         '(A B))
   (form "double binary"
         '((A . 1) (B . 1))
         '(A A B B))
   (form "ternary"
         '((A . 1) (B . 1))
         '(A B A))
   (form "repeated ternary"
         '((A . 1) (B . 1))
         '(A A B A))
   (form "asym rondo"
         '((A . 1) (B . 1) (C . 1) (D . 1) (E . 1))
         '(A B A C A D A E A))
   (form "sym rondo"
         '((A . 1) (B . 1) (C . 1))
         '(A B A C A B A))
   (form "sym rondo"
         '((A . 1) (B . 1) (C . 1))
         '(A B A C A B A))
   (form "arch"
         '((A . 1) (B . 1) (C . 1))
         '(A B C B A))
   (form "typical pop"
         '((I . 1) (V . 1) (C . 1) (M8 . 2) (O . 1))
         '(I V C V C M8 C C O))
   (form "32-bar"
         '((A . 2) (B . 2))
         '(A A B A))
   (form "ABABCB"
         '((A . 1) (B . 1) (C . 1))
         '(A B A B C B))
   (form "ABABCAB"
         '((A . 1) (B . 1) (C . 1))
         '(A B A B C A B))
   (form "ABABCBAB"
         '((A . 1) (B . 1) (C . 1))
         '(A B A B C B A B))
   (form "ABABCABCAB"
         '((A . 1) (B . 1) (C . 1))
         '(A B A B C A B C A B))))

(struct progression (seq) #:transparent)
;; xxx Should look at https://en.wikipedia.org/wiki/List_of_chord_progressions
;; From https://en.wikipedia.org/wiki/Chord_progression

;; xxx i should change this to allow me to write it roman numeral analysis
(define chord-progression/e
  (fin/e
   ;; three chord
   (progression '(0 3 4 4))
   (progression '(0 0 3 4))
   (progression '(0 3 0 4))
   (progression '(0 3 4 3))
   ;; three chord with minor subst
   (progression '(0 2 4 4))
   (progression '(0 0 2 4))
   (progression '(0 2 0 4))
   (progression '(0 2 4 3))
   (progression '(0 3 4 2))
   (progression '(0 1 4))
   (progression '(1 4 0))
   ;; xxx too big
   ;; (progression '(0 3 4 4 0 3 4 0))
   ;; blues
   ;; xxx too big
   ;; (progression '(0 0 0 0 3 3 0 0 4 3 0 0))
   ;; 50s
   (progression '(0 3 4))
   (progression '(0 5 3 4))
   (progression '(0 5 1 4))
   ;; circle
   (progression '(5 1 4 0))
   ;; xxx too big
   ;; (progression '(0 3 6 2 5 1 4 0))
   (progression '(0 4 0))
   (progression '(0 3 4 0))
   (progression '(0 5 1 4))
   ;; harmonizing
   (progression '(0 1 2 3 4))
   (progression '(0 1 0 3 4))
   ;; andalusian
   (progression '(0 6 5 4))
   (progression '(0 2 3 5))))

(define bass-notes/e
  (fin/e (list 0 3 4)))

(define (chord-pulses/e pulse-count chord-count)
  (list-of-length-n-summing-to-k-with-no-zeros/e chord-count pulse-count))

(define/memo (list-of-length-n-summing-to-k-with-no-zeros/e n k)
  (unless (and (positive? n)
               (positive? k)
               (<= n k))
    (error 'list-of-length-n-summing-to-k-with-no-zeros/e
           "Illegal arguments: ~a and ~a"
           n k))
  (cond
   [(= 1 n)
    (const/e (list k))]
   [else
    (dep/e (map/e add1 sub1 (below/e (+ 1 (- k n))))
           (λ (this)
             (list-of-length-n-summing-to-k-with-no-zeros/e
              (- n 1)
              (- k this))))]))

(module+ test
  (printf "16 x 3 =\n")
  (define e (chord-pulses/e 16 3))
  (for ([i (in-naturals)]
        [pd (to-stream e)])
    (printf "~a. ~a\n" i pd)))

(module+ test
  (for* ([k (in-range 1 10)]
         [n (in-range k 10)])
    (define e (chord-pulses/e n k))
    (printf "(f ~a ~a) = ~a\n"
            n k (size e))))

(define/memo (maybe-combine/e a-ts notes)
  (match-define (time-sig _ ts) a-ts)
  (match-define (cons beats-per-bar beat-unit) ts)
  (if (notes . <= . 1)
      (dont-combine/e a-ts notes)
      (cons/e 
       (fin/e (list beat-unit beat-unit)
              (list (* 2 beat-unit)))
       (maybe-combine/e a-ts (- notes 2)))))
(define/memo (dont-combine/e a-ts notes)
  (match-define (time-sig _ ts) a-ts)
  (match-define (cons beats-per-bar beat-unit) ts)
  (if (zero? notes)
      (const/e '())
      (cons/e (const/e (list beat-unit))
              (maybe-combine/e a-ts (sub1 notes)))))

(define/memo (rhythm/e a-ts notes)
  (map/e append* error (maybe-combine/e a-ts notes)))

(module+ test
  (printf "r 4:4 4 =\n")
  (define re (rhythm/e time-sig/ts:4:4 4))
  (for ([i (in-naturals)]
        [r (to-stream re)])
    (printf "~a. ~a\n" i r)))

(define (split-into-measures a-ts ap ns)
  (match-define (time-sig _ ts) a-ts)
  (match-define (cons beats-per-bar beat-unit) ts)
  (define goal (notes-in-bar ts))
  (match-define (accent-pattern _ _ accents) ap)
  (define (consume-notes ns)
    (let loop ([total 0.0] [so-far empty] [ns ns])
      (cond
       [(> total goal)
        (error 'split-into-measures "Notes went over a measure!")]
       [(= total goal)
        (values (reverse so-far) ns)]
       [else
        (match-define (vector note args) (first ns))
        (define new-total (fl+ note total))
        (define (list-ref* l i)
          (if (<= (length l) i)
              #f
              (list-ref l i)))
        (define accented?
          (list-ref* accents (fl->fx (flceiling (fl/ total beat-unit)))))
        (define new-note (list* note args accented?))
        (define new-so-far (cons new-note so-far))
        (loop new-total new-so-far (rest ns))])))
  (define (consume-measures ns)
    (match ns
      ['() '()]
      [_
       (define-values (a-measure more-ns) (consume-notes ns))
       (cons a-measure (consume-measures more-ns))]))
  (consume-measures ns))

(module+ test
  (let ()
    (define ts time-sig/ts:4:4)
    (check-equal?
     (split-into-measures ts (first (hash-ref time-sig->accents ts))
                          (list (vector 0.25 'N)
                                (vector 0.25 'N)
                                (vector 0.25 'N)
                                (vector 0.25 'N)

                                (vector 0.25 'N)
                                (vector 0.25 'N)
                                (vector 0.25 'N)
                                (vector 0.25 'N)))
     (list (list (list* 0.25 'N #t)
                 (list* 0.25 'N #f)
                 (list* 0.25 'N #f)
                 (list* 0.25 'N #f))
           (list (list* 0.25 'N #t)
                 (list* 0.25 'N #f)
                 (list* 0.25 'N #f)
                 (list* 0.25 'N #f))))

    (check-equal?
     (split-into-measures ts (first (hash-ref time-sig->accents ts))
                          (list (vector 0.125 'N) (vector 0.125 'N)
                                (vector 0.125 'N) (vector 0.125 'N)
                                (vector 0.125 'N) (vector 0.125 'N)
                                (vector 0.125 'N) (vector 0.125 'N)))
     (list (list (list* 0.125 'N #t) (list* 0.125 'N #f)
                 (list* 0.125 'N #f) (list* 0.125 'N #f)
                 (list* 0.125 'N #f) (list* 0.125 'N #f)
                 (list* 0.125 'N #f) (list* 0.125 'N #f))))

    (check-exn
     exn:fail?
     (λ ()
       (split-into-measures ts (first (hash-ref time-sig->accents ts))
                            (list (vector 0.125 'N) (vector 0.125 'N)
                                  (vector 0.125 'N) (vector 0.125 'N)
                                  (vector 0.125 'N) (vector 0.125 'N)
                                  (vector 0.125 'N)))))
    (check-exn
     exn:fail?
     (λ ()
       (split-into-measures ts (first (hash-ref time-sig->accents ts))
                            (list (vector 0.125 'N) (vector 0.125 'N)
                                  (vector 0.125 'N) (vector 0.125 'N)
                                  (vector 0.125 'N) (vector 0.125 'N)
                                  (vector 0.125 'N) (vector 0.250 'N)))))))

(define (bithoven-input->composition bi)
  (match-define
   (vector (cons ts (cons ap (cons (cons f cp) cps*crs-s))) bns)
   bi)
  (define scale lazy-scale)
  (define cp-s (progression-seq cp))
  (define btones
    (for/list ([bn (in-list bns)])
      (first (chord-triad (mode scale bn)))))
  (printf "~v\n"
          (vector f cp))
  (define parts
    (for/hasheq ([p (in-list (form-part-lens f))]
                 [chord-pulses*chord-rhythm (in-list cps*crs-s)])
      (match-define (cons chord-pulses chord-rhythm) chord-pulses*chord-rhythm)
      (match-define (cons label len) p)
      (define chord-track
        (split-into-measures
         ts ap
         (append*
          (for/list ([chord (in-list cp-s)]
                     [rhythm (in-list chord-rhythm)])
            (define tones (chord-triad (mode scale chord)))
            (for/list ([r (in-list rhythm)])
              (define melody (select-from-list tones))
              (define harmony (select-from-list tones))
              (define allowed-bass-notes
                (filter (λ (t)
                          (memf (λ (ct) (eq? (car ct) (car t)))
                                tones))
                        btones))
              (when (empty? allowed-bass-notes)
                (error 'bithoven "No bass tone was found in ~v for ~v"
                       btones tones))
              (define bass
                (select-from-list allowed-bass-notes))
              (vector r (list melody harmony bass)))))))
      (values label
              chord-track)))
  (vector (time-sig-ts ts) (accent-pattern-accents ap) (form-pattern f) parts))

(define/memo (part/e ts ap cp measures len)
  (define cp-s (progression-seq cp))
  (define pulses
    (* len measures (accent-pattern-pulses-per-measure ap)))
  (define cp/e
    (chord-pulses/e
     pulses
     (length cp-s)))
  (dep/e
   cp/e
   (λ/memo (cps)
           (traverse/e (λ (cp)
                         (rhythm/e ts (* cp (accent-pattern-notes-per-pulse ap))))
                       cps))))

(define (make-bithoven/e)
  (vec/e
   (dep/e
    time-sig/e
    (λ (ts)
      (dep/e
       (accent-pattern/e ts)
       (λ (ap)
         (dep/e (cons/e form/e chord-progression/e)
                (λ (f*cp)
                  (match-define (cons f cp) f*cp)
                  (define cp-s (progression-seq cp))
                  (define measures-per-part
                    (*
                     ;; Every chord has to get one pulse at least (thus division) and
                     ;; we need a balance of measures (thus ceiling)
                     (let ()
                       (ceiling
                        (/ (length cp-s)
                           (accent-pattern-pulses-per-measure ap))))
                     ;; If a form is long, then don't make each part long
                     (let ()
                       (define pat-length (length (form-pattern f)))
                       (cond
                        [(< pat-length 3) 4]
                        [(< pat-length 5) 2]
                        [else 1]))))
                  (define/memo (this-kind-of-part/e len)
                    (part/e ts ap cp measures-per-part len))
                  (traverse/e
                   (λ (p) (this-kind-of-part/e (cdr p)))
                   (form-part-lens f))))))))
   bass-notes/e))

(define (from-nat/random e)
  (define k (size e))
  (define n (random-natural k))
  (local-require racket/format)
  (define ks (~a k))
  (define ns (~a #:min-width (string-length ks) #:align 'right n))
  (printf "Using n =\n~a of\n~a\n\n" ns ks)
  (from-nat e n))

(define (random-bithoven-input bithoven/e)
  (from-nat/random bithoven/e))

(define (bithoven)
  (define bithoven/e (time (make-bithoven/e)))
  (define bi (random-bithoven-input bithoven/e))
  (printf "bi is ~v\n" bi)
  (bithoven-input->composition bi))

(module+ test
  (require racket/pretty)
  (pretty-print (bithoven)))

(provide bithoven)
