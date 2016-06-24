#lang racket/base
(require racket/match
         racket/fixnum
         racket/runtime-path
         srpnt/apu)

(define-runtime-path tones-path "tones.txt")
(define-values (PULSE TRIANGLE)
  (let ()
    (local-require racket/file
                   racket/string)
    (define PULSE (make-hasheq))
    (define TRIANGLE (make-hasheq))
    (for ([note-line (in-list (file->lines tones-path))]
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
        (define semitone (fx+ (fx- (fx* octave 12) (fx* 4 12)) row))
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

(provide pulse-tone->period
         triangle-tone->period)

(module+ test
  TRIANGLE)
