#lang racket/base
(require racket/runtime-path
         srpnt/music
         srpnt/bithoven
         srpnt/nestration)

(define-runtime-path clip-path "clip.raw.gz")
(define sample-bs (read-sample/gzip 0 4 clip-path))

(define main-track
  (let ()
    (define comp (bithoven))
    (define strat (nestration comp))
    (nes-harmonic comp strat)))
(provide main-track)

(module+ main
  (play-one! main-track))
