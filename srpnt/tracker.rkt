#lang racket/base
(require racket/match
         racket/list
         racket/fixnum
         racket/flonum
         racket/runtime-path
         srpnt/music
         srpnt/music-theory)

(define (part->semicmds me ts p)
  (match-define (cons instru measures) p)
  (define measure-frames (frames-in-bar me ts))
  ;; xxx it would be nice to remove this flatten
  (flatten
   (for/list ([notes (in-list measures)])
     ;; xxx check that all frames are consumed
     (define-values (_ l)
       (for/fold ([frames-remaining measure-frames] [l empty])
                 ([n*t (in-list notes)])
         (match-define (cons note arg) n*t)
         ;; xxx it would be better if these were floats so we could do
         ;; 17,16,17,16 by summing the remainder, rather than doing a
         ;; single rounding in frames-in-note
         (define note-frames
           (fxmin frames-remaining (frames-in-note me note)))
         (values (fx- frames-remaining note-frames)
                 (cons (instru note-frames arg) l))))
     (reverse l))))

(define list-#f (list #f))
(define (extend-with-#f v)
  (match v
    ['() list-#f]
    [(cons a d) v]))

(define (apply-mapish f ls)
  (match ls
    [(or '() (list '() ...))
     '()]
    [(list (cons a d)
           ...)
     (cons (f a)
           (apply-mapish f d))]
    [_
     ;; xxx When this is false, I add more to the end of one track
     ;; when it isn't balanced, but this has the problem of causing
     ;; empty patches. I think the real thing to do is ensure that
     ;; each track doesn't go under/over a measure's frames
     (if #t
         '()
         (apply-mapish f (map extend-with-#f ls)))]))

(define (combine-semicmds scs)
  ;; xxx this sucks
  (match scs
    [(list #f ...)
     (cmd:frame* #f #f #f #f #f #f)]
    [(list (? wave:pulse? p1) #f ...)
     (cmd:frame* p1 #f #f #f #f #f)]
    [(list (? wave:pulse? p1) (? wave:pulse? p2) #f ...)
     (cmd:frame* p1 p2 #f #f #f #f)]
    [(list (? wave:pulse? p1) (? wave:pulse? p2) (? wave:triangle? t) #f ...)
     (cmd:frame* p1 p2 t #f #f #f)]
    [(list (? wave:pulse? p1) (? wave:pulse? p2) (? wave:noise? n))
     (cmd:frame* p1 p2 #f n #f #f)]
    [(list (? wave:pulse? p1) (? wave:pulse? p2) (? wave:triangle? t) (? wave:noise? n))
     (cmd:frame* p1 p2 t n #f #f)]
    [(list #f #f (? wave:noise? n))
     (cmd:frame* #f #f #f n #f #f)]
    [(list #f #f #f (? wave:noise? n))
     (cmd:frame* #f #f #f n #f #f)]))


(define (combine-parts part-semicmds)
  (apply-mapish combine-semicmds part-semicmds))

(define (song->commands #:me me #:ts ts parts)
  (combine-parts (map (λ (p) (part->semicmds me ts p)) parts)))

(define (chorded-song->commands #:me me
                                #:ts ts
                                #:drum drum
                                #:drum-measure dm
                                #:instruments iv
                                #:measures ms)
  (song->commands
   #:me me
   #:ts ts
   (append
    (for/list ([i*o (in-vector iv)]
               [in (in-naturals)])
      (match-define (cons i inst-octave) i*o)
      (cons i
            (for/list ([m (in-list ms)])
              (for/list ([n (in-list m)])
                (match-define (list* note tones accent?) n)
                (match-define (cons tone-name note-doctave) (list-ref tones in))
                (define tone
                  (string->symbol
                   (format "~a~a" tone-name (+ inst-octave note-doctave))))
                (list* note tone accent?)))))
    (list (cons drum
                (for/list ([m (in-list ms)]
                           [i (in-naturals)])
                  dm))))))

;; xxx
(provide (all-defined-out))
