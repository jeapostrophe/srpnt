#lang racket/base
(require racket/match
         racket/runtime-path
         srpnt/player
         srpnt/apu
         srpnt/bytes-player)

(define (cmd:frame* p1 p2 t n ld rd)
  (cmd:frame (or p1 off-wave:pulse)
             (or p2 off-wave:pulse)
             (or t off-wave:triangle)
             (or n off-wave:noise)
             (or ld off-wave:dmc)
             (or rd off-wave:dmc)))
(define (cmd:hold* frames c)
  (for/list ([f (in-range frames)])
    c))
(define (cmd:hold*f frames cf)
  (for/list ([f (in-range frames)])
    (cf f)))

(define-runtime-path keys-path "keys.txt")
(define-values (PULSE TRIANGLE)
  (let ()
    (local-require racket/file
                   racket/string)
    (define PULSE (make-hasheq))
    (define TRIANGLE (make-hasheq))
    (for ([note-line (in-list (file->lines keys-path))]
          [row (in-naturals)])
      (match-define (cons note-s freq+offs) (string-split note-line))
      (define note-l
        (for/list ([note (in-list (string-split note-s "/"))])
          (regexp-replace #rx"♭"
                          (regexp-replace #rx"♯" note "#")
                          "b")))
      (define periods
        (for/list ([fo (in-list freq+offs)]
                   [i (in-naturals)]
                   #:when (even? i))
          (define f (string->number fo))
          (cons (pulse-freq->period f)
                (triangle-freq->period f))))
      (for ([p (in-list periods)]
            [octave (in-naturals)])
        (match-define (cons pul tri) p)
        (define semitone (+ (- (* octave 12) (* 4 12)) row))
        (define stn semitone)
        (when pul
          (hash-set! PULSE stn pul))
        (when tri
          (hash-set! TRIANGLE stn tri))
        (for ([n (in-list note-l)])
          (define no (string->symbol (format "~a~a" n octave)))
          (when pul
            (hash-set! PULSE no pul))
          (when tri
            (hash-set! TRIANGLE no tri)))))
    (values PULSE TRIANGLE)))

(define (pulse-tone->period tone)
  (hash-ref PULSE tone
            (λ () (error 'pulse-tone->period "The pulse can't play ~v\n"
                         tone))))
(define (triangle-tone->period tone)
  (hash-ref TRIANGLE tone
            (λ () (error 'triangle-tone->period "The triangle can't play ~v\n"
                         tone))))

(provide
 pulse-freq->period
 triangle-freq->period
 samples-per-buffer
 read-sample/gzip
 read-sample/path
 read-sample/port
 (all-from-out srpnt/player)
 cmd:frame*
 cmd:hold*
 cmd:hold*f
 pulse-tone->period
 triangle-tone->period)
