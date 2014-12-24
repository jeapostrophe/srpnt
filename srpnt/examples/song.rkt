#lang racket/base
(require racket/runtime-path
         srpnt/music
         srpnt/bithoven
         srpnt/nestration)

(define-runtime-path clip-path "clip.raw.gz")
(define sample-bs (read-sample/gzip 0 4 clip-path))

(define (make-main-track)
  (define comp (bithoven))
  (define strat (nestration comp))
  (nes-harmonic comp strat))

(define main-track
  (make-main-track))
(provide main-track)

(module+ main
  (let loop ()
    (with-handlers ([exn:fail? void])
      (play-one! (make-main-track)))
    (loop)))
