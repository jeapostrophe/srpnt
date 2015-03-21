#lang racket/base
(require racket/runtime-path
         racket/match
         srpnt/music
         srpnt/bithoven
         srpnt/nestration)

(define-runtime-path clip-path "clip.raw.gz")
(define sample-bs (read-sample/gzip 0 4 clip-path))

(define (make-main-track)
  (define comp (bithoven))
  (define strat (nestration comp))
  (cons comp strat))

(define main-track
  (let ()
    (match-define (cons comp strat) (make-main-track))
    (nes-harmonic comp strat)))
(provide main-track)

(define audio (make-main-track))
(provide audio)

(module+ main
  (let loop ()
    (with-handlers ([exn:fail? void])
      (play-one! (make-main-track)))
    (loop)))
