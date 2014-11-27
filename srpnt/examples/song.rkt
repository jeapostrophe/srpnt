#lang racket/base
(require racket/runtime-path
         srpnt/music
         srpnt/bithoven
         srpnt/nestration)

(define-runtime-path clip-path "clip.raw.gz")
(define sample-bs (read-sample/gzip 0 4 clip-path))

(define main-track
  (composition->track (bithoven) (nestration)))
(provide main-track)

(module+ main
  (play-one! main-track))
