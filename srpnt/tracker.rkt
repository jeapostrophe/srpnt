#lang racket/base
(require racket/match
         racket/list
         racket/fixnum
         racket/contract/base
         racket/flonum
         srpnt/synth
         srpnt/nestration/instrument
         srpnt/music-theory)

(define (cmd:hold* frames c)
  (for/list ([f (in-range frames)])
    c))

(define (part->semicmds me ts p)
  (match-define (cons instru measures) p)
  (define bar-notes-goal (notes-in-bar ts))
  (define measure-frames (frames-in-bar me ts))
  ;; xxx it would be nice to remove this flatten
  (flatten
   (for/list ([notes (in-list measures)])
     (define-values (left-overs final-fragments notes-total l)
       (for/fold ([frames-remaining measure-frames]
                  [frame-fragments 0.0]
                  [note-total 0.0]
                  [l empty])
                 ([n*t (in-list notes)]
                  [i (in-naturals)])
         (match-define (cons note arg) n*t)
         (define enote-frames.0
           (fl+ frame-fragments (frames-in-note.0 me note)))
         (define note-frames.0
           (flfloor enote-frames.0))
         (define note-frames
           (if (fx= i (fx- (length notes) 1))
               frames-remaining
               (fl->fx note-frames.0)))
         (values (fx- frames-remaining note-frames)
                 (fl- enote-frames.0 note-frames.0)
                 (fl+ note-total note)
                 (cons (if arg
                           (instru note-frames arg)
                           (cmd:hold* note-frames #f))
                       l))))
     (unless (fl< (flabs (fl- bar-notes-goal notes-total)) 0.000001)
       (error 'part->semicmds "Notes did not fill measure: ~v should be ~v for ~v"
              notes-total bar-notes-goal notes))
     (unless (fx= 0 left-overs)
       (error 'part->semicmds "Did not exactly consume frames: ~v\n" left-overs))
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
     (error 'apply-mapish "Tracks are not compatible: ~v\n" ls)]))

(define (combine-semicmds scs)
  (match-define (list p1 p2 t1 t2 n) scs)
  (synth:frame p1 p2 t1 t2 n #f #f))

(define (combine-parts part-semicmds)
  (apply-mapish combine-semicmds part-semicmds))

(define (song->commands #:me me #:ts ts parts)
  (combine-parts (map (λ (p) (part->semicmds me ts p)) parts)))

(define (chorded-song->commands #:me me
                                #:ts ts
                                #:drum drum
                                #:drum-measures dms
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
                (match (list-ref tones in)
                  [(cons tone-name note-doctave)
                   (define tone
                     (string->symbol
                      (format "~a~a" tone-name (+ inst-octave note-doctave))))
                   (list* note tone accent?)]
                  [#f
                   (cons note #f)])))))
    (list (cons drum
                dms)))))

(provide
 (contract-out
  [chorded-song->commands
   (-> #:me
       metronome/c
       #:ts
       time-sig/c
       #:drum
       instrument:drums/c
       #:drum-measures
       (listof
        (listof
         (cons/c note/c
                 (integer-in 0 2))))
       #:instruments
       (vector/c
        (cons/c instrument:pulse-or-triangle/c exact-nonnegative-integer?)
        (cons/c instrument:pulse-or-triangle/c exact-nonnegative-integer?)
        (cons/c instrument:pulse-or-triangle/c exact-nonnegative-integer?)
        (cons/c instrument:pulse-or-triangle/c exact-nonnegative-integer?))
       #:measures
       (listof
        (listof 
         (cons/c note/c
                 (cons/c
                  (listof (or/c #f (cons/c symbol? exact-nonnegative-integer?)))
                  boolean?))))
       (listof synth:frame?))]))
