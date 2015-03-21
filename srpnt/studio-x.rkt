#lang racket/base
(require racket/match
         racket/format
         racket/port
         charterm
         lux
         lux/chaos/charterm
         srpnt/player
         srpnt/nestration
         srpnt/bithoven)

(struct studio (si ci c ni n t)
        #:methods gen:word
        [(define (word-fps w)
           60.0)
         (define (word-label s ft)
           "NEStudio")
         (define (word-event w e)
           (match-define (studio si ci c ni n t) w)
           (match e
             [(and (? char?) (? char-numeric? e))
              (kill-thread t)
              (studio (string->number (string e)) ci c #f #f #f)]
             [#\r
              (kill-thread t)
              w]
             [#\n
              (kill-thread t)
              (studio si ci c #f #f #f)]
             [#\c
              (kill-thread t)
              (studio si #f #f #f #f #f)]
             ['escape
              #f]
             [else
              w]))
         (define (word-output w)
           (match-define (studio si ci _ ni _ _) w)
           (lambda ()
             (define-values (width height) (charterm-screen-size))
             (charterm-clear-screen)
             (define status-y (- height 1))
             (charterm-cursor 1 status-y)
             (charterm-inverse)
             (charterm-display #:width width
                               "NEStudio")
             (charterm-normal)
             (define DISP
               (append
                (list (~a #:max-width width "c = " ci)
                      (~a #:max-width width "n = " ni)
                      "")
                (for/list ([i (in-naturals)]
                           [s (in-list styles)])
                  (format "~a - switch to ~a" i (style-name s)))
                (list ""
                      "r - restart song"
                      (format "n - new nestration (~a)"
                              (style-name (list-ref styles si)))
                      "c - new composition"
                      "ESC - Quit")))
             (for ([y (in-naturals 1)]
                   [h (in-list DISP)])
               (charterm-cursor 1 y)
               (charterm-display h))
             (charterm-cursor 0 0)))
         (define (word-tick w)
           (match w
             [(studio si _ #f _ _ _)
              (define-values (ci c) (bithoven+idx))
              (studio si ci c #f #f #f)]
             [(studio si ci c _ #f _)
              (define n/e (nestration/e #:style (list-ref styles si) c))
              (define-values (ni n)
                (nestration+idx #:n/e n/e c))
              (studio si ci c ni n #f)]
             [(studio si ci c ni n #f)
              (studio si ci c ni n
                      ;; xxx change this model to not use a thread
                      (thread
                       (λ ()
                         (parameterize
                             ([current-output-port (open-output-nowhere)])
                           (play-one! (nes-harmonic c n))))))]
             [(studio si ci c ni n t)
              (if (thread-dead? t)
                  (studio si ci c ni n #f)
                  w)]))])

(define (studio-it)
  (define s (studio 0 #f #f #f #f #f))
  (fiat-lux s))

(module+ main
  (call-with-chaos
   (make-charterm)
   (λ ()
     (studio-it))))
