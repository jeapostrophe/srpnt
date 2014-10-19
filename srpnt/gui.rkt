#lang racket/base
(require racket/math
         racket/gui/base
         racket/file
         racket/class
         racket/fixnum
         racket/performance-hint
         srpnt/bytes-player)

(define OSC-W samples-per-buffer)
(define OSC-Hshort (fx* 2 16))
(define OSC-Htall 128)
(define OSC-MARGIN 8)
(define OSCSshort (+ 1 1 1 1))
(define OSCStall (+ 1 1 1 1))

(define W
  (+ OSC-MARGIN OSC-W OSC-MARGIN))
(define H
  (+ OSC-MARGIN
     (* (+ OSC-Hshort OSC-MARGIN) OSCSshort)
     (* (+ OSC-Htall OSC-MARGIN) OSCStall)
     OSC-MARGIN))

(define (go p)
  (define channels (+ OSCSshort OSCStall))
  (define expected-length (fx* channels samples-per-buffer))
  (define bs (make-bytes expected-length 0))
  (define (paint! c dc)
    (send dc set-background "black")
    (send dc set-text-foreground "yellow")
    (send dc set-pen "yellow" 1 'solid)
    (send dc clear)

    (define start-y 0)

    (begin-encourage-inline
     (define (draw-osc! label off start-x)
       (send dc draw-text label OSC-MARGIN start-y)
       (for/fold ([last-x -1] [last-y 0])
                 ([i (in-range samples-per-buffer)])
         (define this-x i)
         (define this-y (fx- (bytes-ref bs (fx+ off (fx* i channels))) start-x))
         (send dc draw-line
               (fx+ OSC-MARGIN last-x) (fx- start-y last-y)
               (fx+ OSC-MARGIN this-x) (fx- start-y this-y))
         (values this-x this-y))))

    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Hshort)))
    (draw-osc! "Pulse-1" 0 0)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Hshort)))
    (draw-osc! "Pulse-2" 1 0)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Hshort)))
    (draw-osc! "Triangle" 2 0)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Hshort)))
    (draw-osc! "Noise" 3 0)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Htall)))
    (draw-osc! "DMC-L" 4 0)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Htall)))
    (draw-osc! "DMC-R" 5 0)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Htall)))
    (draw-osc! "Combined-L" 6 128)
    (set! start-y (fx+ start-y (fx+ OSC-MARGIN OSC-Htall)))
    (draw-osc! "Combined-R" 7 128))
  (define f (new frame% [label "SRPNT"] [width W] [height H]
                 [min-width W] [min-height H]))
  (define c (new canvas% [parent f] [paint-callback paint!]
                 [min-width W] [min-height H]))

  (thread
   (λ ()
     (let loop ()
       (with-handlers ([exn:fail?
                        (λ (x)
                          (eprintf "ERR: ~a\n" (exn-message x)))])
         (yield (filesystem-change-evt p))
         (define new-bs (file->bytes p))
         (define new-bs-len (bytes-length new-bs))
         (cond
          [(= new-bs-len expected-length)
           (set! bs new-bs)
           (send c refresh-now)]
          [else
           (eprintf "UNDERFLOW: ~a\n" new-bs-len)]))
       (loop))))

  (send f show #t))

(module+ main
  (require racket/cmdline)
  (command-line #:program "gui"
                #:args (f)
                (go f)))
