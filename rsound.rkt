#lang racket
(require rsound)

;;  freq = 111860.8 / (raw period + 1)
(define nes-pulse-period-min 0)
(define nes-pulse-period-max (sub1 (expt 2 11)))

(define (freq->nes-pulse freq)
  (round (- (/ 111860.8 freq) 1)))
(define (nes-pulse->freq raw-period)
  (unless (<= nes-pulse-period-min raw-period nes-pulse-period-max)
    (error 'nes-pulse-freq "Out of range"))
  (/ 111860.8 (+ raw-period 1)))

(define (freq->piano f)
  (+ (* 12 (log2 (/ f 440))) 49))
(define (piano->freq n)
  (* (expt 2 (/ (- n 49) 12)) 440))

(module+ test
  (freq->nes-pulse 440)

  (freq->piano (nes-pulse->freq nes-pulse-period-min))
  (freq->piano (nes-pulse->freq nes-pulse-period-max)))

(define (log2 x)
  (/ (log x) (log 2)))
(define (freq->midi-pitch f)
  (+ 69 (* 12 (log2 (/ f 440)))))

(module+ test-x
  (for ([pr (in-range nes-pulse-period-min (add1 nes-pulse-period-max))])
    (define freq (nes-pulse->freq pr))
    (define mp (freq->midi-pitch freq))
    (printf "~v -> ~v -> ~v\n" pr freq mp)))

(define (piano->nes->freq n)
  (nes-pulse->freq (freq->nes-pulse (piano->freq n))))

(define (nes-pulse-duty duty-i)
  (match duty-i
    [0 0.125]
    [1 0.25]
    [2 0.5]
    [3 0.75]))

(define (nes-tone n)
  (define quarter-per-min 273)
  (define quarter-per-seconds (/ quarter-per-min 60))
  (define seconds-per-quarter (/ 1 quarter-per-seconds))
  (signal->rsound
   (round (* seconds-per-quarter 44100))
   (network ()
            [lfo <= pulse-wave
                 (nes-pulse-duty 2)
                 (piano->nes->freq n)]
            [out = lfo])))
(module+ main
  (define C4 40)
  (define E4 44)
  (define G4 47)
  (define B4 51)
  (define r
    (rs-append*
     (list (nes-tone C4)
           (nes-tone E4)
           (nes-tone G4)
           (nes-tone B4))))
  (rs-write r "t.wav"))
