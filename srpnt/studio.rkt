#lang racket/base
(require racket/match
         srpnt/player
         srpnt/band
         srpnt/nestration
         srpnt/mixer
         srpnt/speaker)

(define (load-audio mp)
  (define ns (make-base-namespace))
  (namespace-attach-module (current-namespace) 'racket/base ns)
  (namespace-attach-module (current-namespace) 'srpnt/player ns)
  (parameterize ([current-namespace ns])
    (namespace-require `(file ,mp))
    (namespace-variable-value 'audio)))

(define (save! file-path save-p)
  (define (m)
    (mixer:standard
     (speaker:fork
      (speaker:real)
      (speaker:file save-p))))
  (once! file-path m))

(define (once! file-path m)
  (define a (load-audio file-path))
  (match-define (cons c n) a)
  (play-one!
   #:mixer m
   (compile-song c n)))

(define (studio file-path other-files)
  (define (make-playing-thread)
    (thread
     (λ () (once! file-path #f))))
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
  (define save-p #f)
  (command-line
   #:program "studio"
   #:once-each
   [("-s" "--save") p "Save the audio to path p"
    (set! save-p p)]
   #:args (file-path . other-files)
   (cond
     [save-p
      (save! file-path save-p)]
     [else
      (studio file-path other-files)])))
