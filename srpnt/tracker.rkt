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
                 (cons (instru note-frames arg) l))))
     (unless (fl= bar-notes-goal notes-total)
       (error 'part->semicmds "Notes did not fill measure: ~v should be ~v"
              notes-total bar-notes-goal))
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
  (let loop ([p1 #f] [p2 #f] [t #f] [n #f] [scs scs])
    (match scs
      ['()
       (cmd:frame* p1 p2 t n #f #f)]
      [(cons #f scs)
       (loop p1 p2 t n scs)]
      [(cons (? wave:pulse? new-p) scs)
       (loop new-p p1 t n scs)]
      [(cons (? wave:triangle? new-t) scs)
       (loop p1 p2 new-t n scs)]
      [(cons (? wave:noise? new-n) scs)
       (loop p1 p2 t new-n scs)])))

(define (combine-parts part-semicmds)
  (apply-mapish combine-semicmds part-semicmds))

;; xxx make this more like a tracker that takes independent tracks and
;; combines them with their own note lengths
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

;; xxx look at http://famitracker.com/wiki/index.php?title=7xy and
;; http://nes-audio.com/manuals/nijuu/nijuu_manual.html for effects

;; xxx look at
;; http://famitracker.com/wiki/index.php?title=Pattern_editor for
;; tracking ideas

;; xxx cleanup
(provide (all-defined-out))
