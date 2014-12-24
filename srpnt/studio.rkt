#lang racket/base
(require racket/match
         srpnt/player
         srpnt/nestration)

(define (load-audio mp)
  (define ns (make-base-namespace))
  (namespace-attach-module (current-namespace) 'racket/base ns)
  (namespace-attach-module (current-namespace) 'srpnt/player ns)
  (parameterize ([current-namespace ns])
    (namespace-require `(file ,mp))
    (namespace-variable-value 'audio)))

(define (studio file-path other-files)
  (define (make-playing-thread)
    (thread
     (λ ()
       (define a (load-audio file-path))
       (match-define (cons c n) a)
       (play-one! (nes-harmonic c n)))))
  (define (make-fs-evts)
    (for/list ([fp (in-list (cons file-path other-files))])
      (filesystem-change-evt fp)))
  (let loop ([pt always-evt]
             [fs-evts (make-fs-evts)])
    (define (reload-handler _)
      (kill-thread pt)
      (for-each filesystem-change-evt-cancel fs-evts)
      (loop pt (make-fs-evts)))
    (apply
     sync
     (handle-evt pt
                 (λ (_)
                   (loop (make-playing-thread) fs-evts)))
     (for/list ([fs-evt (in-list fs-evts)])
       (handle-evt fs-evt reload-handler)))))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "studio"
   #:args (file-path . other-files)
   (studio file-path other-files)))
