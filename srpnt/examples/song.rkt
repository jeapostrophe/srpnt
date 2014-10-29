#lang racket/base
(require racket/match
         racket/list
         racket/fixnum
         racket/flonum
         racket/runtime-path
         srpnt/music
         srpnt/music-theory
         srpnt/tracker)
(module+ test
  (require rackunit))

(define-runtime-path clip-path "clip.raw.gz")
(define sample-bs (read-sample/gzip 0 4 clip-path))

(define bad-dudes-ex
  (cmd:hold* 10
             (cons
              (cmd:hold*f 40
                          (λ (f)
                            (cmd:frame* (wave:pulse (modulo f 3)
                                                    (pulse-freq->period 261.626)
                                                    7)
                                        #f #f #f #f #f)))
              (cmd:hold* 20
                         (cmd:frame* #f #f #f #f #f #f)))))

(define alien3-example
  (cmd:hold* 10
             (cons
              (for/list ([vol (in-list (list 0 7 6 5 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0))]
                         [dperiod (in-list (list -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 4 4 -3 -4 -4 -3 -4 -4 3 4 4 3 4 4 -3 -4 -4 -3 -4 -4 3 4 4 -3 -4 -4 -3))]
                         [duty (in-list (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1  0 0 0 0 0 0 0 0 1))])
                (cmd:frame* (wave:pulse duty
                                        (+ (pulse-freq->period 261.626) dperiod)
                                        vol)
                            #f #f #f #f #f))
              (cmd:hold* 20
                         (cmd:frame* #f #f #f #f #f #f)))))

(define gremlins2-example
  (cmd:hold* 10
             (list*
              (cmd:frame* (wave:pulse 2 (pulse-freq->period 261.626) 12)
                          #f #f #f #f #f)
              (cmd:hold* 3
                         (cmd:frame* (wave:pulse 1 (pulse-freq->period 261.626) 12)
                                     #f #f #f #f #f))
              (cmd:hold* 4
                         (cmd:frame* (wave:pulse 1 (pulse-freq->period 261.626) 11)
                                     #f #f #f #f #f))
              (cmd:hold* 4
                         (cmd:frame* (wave:pulse 1 (pulse-freq->period 261.626) 1)
                                     #f #f #f #f #f))
              (cmd:hold* 4
                         (cmd:frame* (wave:pulse 1 (pulse-freq->period 261.626) 3)
                                     #f #f #f #f #f))
              (cmd:hold* 16
                         (cmd:frame* #f #f #f #f #f #f)))))

(define noise-test-suite
  (for*/list ([short? (in-list (list #f #t))]
              [noise-p (in-range 16)])
    (cmd:hold* 15
               (cmd:frame* #f #f #f
                           (wave:noise short? noise-p 4)
                           #f #f))))

(define initial-test
  (list*
   (cmd:hold* 30
              (cmd:frame* (wave:pulse 0 (pulse-freq->period 261.626) 4)
                          #f #f #f #f #f))
   (cmd:hold* 30
              (cmd:frame* #f
                          (wave:pulse 2 (pulse-freq->period 440.00) 4)
                          #f #f #f #f))
   (cmd:hold* 15
              (cmd:frame* #f #f
                          (wave:triangle #t (triangle-freq->period 440.00))
                          #f #f #f))
   (cmd:hold* 15
              (cmd:frame* #f #f #f
                          (wave:noise #f 0 4)
                          #f #f))
   ;; XXX This is not a convenient interface, need a way to
   ;; fuse a DMC across the other frames
   (cmd:hold*f 55
               (λ (f)
                 (define s (fx* f samples-per-buffer))
                 (cmd:frame* #f #f #f #f
                             (wave:dmc sample-bs s)
                             #f)))))

;; 3, 4, 8 sound good
;; 9 is crunchy
;; 7 and C are okay

(define-syntax-rule (noise-line [len short? period vol] ...)
  (list* (cmd:hold* len (wave:noise short? period vol)) ...))

(define bass-drum
  (noise-line [1 #f 9 10] [2 #f 9 7] [3 #f 9 4] [3 #f 9 3] [4 #f 9 2]))

(define snare-drum1
  (noise-line [1 #f 7 11]
              [1 #f 7 9]
              [2 #f 7 8]
              [2 #f 7 7]
              [3 #f 7 4]
              [3 #f 7 3]
              [3 #f 7 2]))

(define snare-drum2
  (noise-line [1 #f 7 11]
              [1 #f 7 9]
              [1 #f 7 8]
              [1 #f 7 7]
              [1 #f 7 6]
              [2 #f 7 4]
              [2 #f 7 3]
              [4 #f 7 2]))

(define closed-hihat
  (noise-line [1 #f #xC 4]
              [2 #f #xC 3]
              [4 #f #xC 2]
              [4 #f #xC 1]))

(define loose-hihat
  (noise-line [1 #f #xC 7]
              [2 #f #xC 5]
              [4 #f #xC 4]
              [2 #f #xC 2]
              [4 #f #xC 2]
              [2 #f #xC 1]))

(define open-hihat
  (noise-line [1 #f #xC 6]
              [1 #f #xC 5]
              [3 #f #xC 4]
              [2 #f #xC 3]
              [4 #f #xC 2]
              [4 #f #xC 1]))

;; xxx more real code

;; xxx add different kinds of accents
;; xxx generate drum beats
;;  - https://en.wikipedia.org/wiki/Drum_beat from accents & time-sigs
;;  - http://retrogameaudio.tumblr.com/post/19088836599/nes-audio-asterix-noise-instruments

(define (i:pulse duty volume)
  (λ (frames tone*accent?)
    (match-define (cons tone accent?) tone*accent?)
    (define evolume (if accent? (fxmin 15 (fx+ 1 volume)) volume))
    (cmd:hold* frames
               (wave:pulse duty (pulse-tone->period tone) evolume))))

(define (i:triangle)
  (λ (frames tone*accent?)
    (match-define (cons tone accent?) tone*accent?)
    (cmd:hold* frames
               (wave:triangle #t (triangle-tone->period tone)))))

(define (i:pulse-plucky pluck% duty volume)
  (λ (frames tone*accent?)
    (match-define (cons tone accent?) tone*accent?)
    (define evolume (if accent? (fxmin 15 (fx+ 1 volume)) volume))
    (define per (pulse-tone->period tone))
    (define pluck-frames (fl->fx (flceiling (fl* (fx->fl frames) pluck%))))
    (define unpluck-frames (fx- frames pluck-frames))
    (define unpluck-frames-third (fxquotient unpluck-frames 3))
    (define unpluck-frames-final-third
      (fx- unpluck-frames (fx* unpluck-frames-third 2)))
    (list*
     (cmd:hold* pluck-frames
                (wave:pulse duty per evolume))
     (cmd:hold* unpluck-frames-third
                (wave:pulse (fxmax 0 (fx- duty 1)) per evolume))
     (cmd:hold* unpluck-frames-third
                (wave:pulse (fxmax 0 (fx- duty 1)) per (fxquotient evolume 2)))
     (cmd:hold* unpluck-frames-final-third
                (wave:pulse (fxmax 0 (fx- duty 1)) per (fxquotient evolume 4))))))

(define (i:pulse-slow-mod how-many duty volume)
  (λ (frames tone*accent?)
    (match-define (cons tone accent?) tone*accent?)
    (define evolume (if accent? (fxmin 15 (fx+ 1 volume)) volume))
    (define per (pulse-tone->period tone))
    (define part-frames (fl->fx (flceiling (fl/ (fx->fl frames) (fx->fl how-many)))))
    (define-values (_ l)
      (for/fold ([remaining frames] [l empty])
                ([n (in-range how-many)])
        (values
         (fx- remaining part-frames)
         (cons l
               (cmd:hold* (fxmin remaining part-frames)
                          (wave:pulse (fxmax 0 (if (even? n) duty (fx- duty 1)))
                                      per evolume))))))
    l))

(define (i:drum which-v)
  (λ (frames which-n)
    (define which (vector-ref which-v which-n))
    (cmd:ensure frames which)))

(define example-song
  (song->commands
   #:me (cons 0.25 160)
   #:ts ts:4:4
   (list
    (cons (i:pulse-plucky 0.25 2 8)
          (list
           (list (list* 0.25 'C4 #f)
                 (list* 0.25 'E4 #t)
                 (list* 0.25 'G4 #f)
                 (list* 0.25 'A4 #f))))
    (cons (i:pulse-slow-mod 16 2 4)
          (list
           (list (list* 0.25 'E4 #f)
                 (list* 0.25 'G4 #f)
                 (list* 0.25 'A4 #f)
                 (list* 0.25 'C4 #t))))
    (cons (i:triangle)
          (list
           (list (list* 0.25 'G3 #f)
                 (list* 0.25 'A3 #f)
                 (list* 0.25 'C3 #f)
                 (list* 0.25 'E3 #f))))
    (cons (i:drum (vector closed-hihat
                          bass-drum
                          snare-drum2))
          (list
           (list (cons 0.125 0)
                 (cons 0.125 0)
                 (cons 0.125 1)
                 (cons 0.125 0)
                 (cons 0.125 0)
                 (cons 0.125 0)
                 (cons 0.125 2)
                 (cons 0.125 0)))))))

(define libbys-song
  (let ()
    (define s
      (scale-diatonic-major 'C))
    (define base-octave 4)
    (chorded-song->commands
     #:me (cons 0.25 140)
     #:ts ts:4:4
     #:drum (i:drum (vector closed-hihat
                            bass-drum
                            snare-drum2))
     #:drum-measure
     (list (cons 0.125 0)
           (cons 0.125 0)
           (cons 0.125 1)
           (cons 0.125 0)
           (cons 0.125 0)
           (cons 0.125 0)
           (cons 0.125 2)
           (cons 0.125 0))
     #:instruments
     (vector (cons (i:pulse-plucky 0.25 2 8) (fx+ base-octave 2))
             (cons (i:pulse-slow-mod 16 2 4) base-octave)
             (cons (i:triangle) (fx- base-octave 2)))
     #:measures
     (list
      ;; Line 1
      (list
       (list* 0.25 (chord-inversion (chord-seventh (mode s 0)) 0) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 0)) 1) #t)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 0)) 2) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 0)) 3) #t))
      (list
       (list* 0.25 (chord-inversion (chord-seventh (mode s 1)) 0) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 1)) 1) #t)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 1)) 2) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 1)) 3) #t))
      (list
       (list* 0.25 (chord-inversion (chord-seventh (mode s 4)) 0) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 4)) 1) #t)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 4)) 2) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 4)) 3) #t))
      ;; Line 2
      (list
       (list* 0.25 (octave-delta (chord-inversion (chord-seventh (mode s 0)) 0) 1) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 0)) 2) #t)
       (list* 0.50 (chord-inversion (chord-seventh (mode s 0)) 0) #f))
      (list
       (list* 0.25 (chord-inversion (chord-seventh (mode s 5)) 0) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 5)) 1) #t)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 1)) 2) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 1)) 3) #t))
      (list
       (list* 0.25 (chord-inversion (chord-seventh (mode s 4)) 0) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 4)) 1) #t)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 4)) 0) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 4)) 1) #t))
      ;; Line 3
      (list
       (list* 0.25 (chord-inversion (chord-seventh (mode s 0)) 0) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 0)) 2) #t)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 2)) 0) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 2)) 2) #t))
      (list
       (list* 0.50 (chord-inversion (chord-seventh (mode s 5)) 0) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 1)) 2) #t)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 1)) 3) #f))
      (list
       (list* 0.25 (chord-inversion (chord-seventh (mode s 4)) 0) #f)
       (list* 0.25 (chord-inversion (chord-seventh (mode s 4)) 1) #t)
       (list* 0.50 (chord-inversion (chord-seventh (mode s 4)) 2) #f))))))

(module+ test
  (printf "0.75 is ~a\n"
          (frames-in-note (cons 0.25 116) 0.75))
  (printf "0.125 is ~a\n"
          (frames-in-note (cons 0.25 116) 0.125))
  (printf "6*0.125 is ~a\n"
          (* 6 (frames-in-note (cons 0.25 116) 0.125))))

(define 237:Do-What-Is-Right
  (let ()
    (define s
      (scale-diatonic-major 'G))
    (define base-octave 4)
    (chorded-song->commands
     #:me (cons 0.25 116)
     #:ts ts:3:4
     #:drum (i:drum (vector closed-hihat
                            bass-drum
                            snare-drum2))
     #:drum-measure
     (list (cons 0.125 2)
           (cons 0.125 0)
           (cons 0.125 0)
           (cons 0.125 0)
           (cons 0.125 0)
           (cons 0.125 0))
     #:instruments
     (vector (cons (i:pulse-plucky 0.25 2 8) (fx+ base-octave 1))
             (cons (i:pulse-slow-mod 16 2 4) base-octave)
             (cons (i:triangle) (fx- base-octave 1)))
     #:measures
     (let ()
       (define phrase1
         (list
          ;; 1
          (list
           (list* 0.375 '((B . 0) (D . 0) (G . 0)) #f)
           (list* 0.125 '((A . 0) (C . 0) (G . 0)) #f)
           (list* 0.250 '((G . 0) (B . 0) (G . 0)) #f))
          ;; 2
          (list
           (list* 0.500 '((B . 0) (D . 0) (G . 0)) #f)
           (list* 0.250 '((G . 0) (B . 0) (G . 0)) #f))
          ;; 3
          (list
           (list* 0.250 '((A . 0) (C . 0) (D . 0)) #f)
           (list* 0.250 '((G . 0) (B . 0) (D . 0)) #f)
           (list* 0.250 '((A . 0) (C . 0) (D . 0)) #f))
          ;; 4
          (list
           (list* 0.250 '((B . 0) (D . 0) (G . 0)) #f)
           (list* 0.500 '((G . 0) (B . 0) (G . 0)) #f))
          ;; 5
          (list
           (list* 0.375 '((G . 0) (E . 0) (C . 0)) #f)
           (list* 0.125 '((F# . 0) (D . 0) (C . 0)) #f)
           (list* 0.250 '((E . 0) (C . 0) (C . 0)) #f))
          ;; 6
          (list
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((G . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((B . 0) (D . 0) (G . 0)) #f))
          ;; 7
          (list
           (list* 0.375 '((A . 0) (C . 0) (D . 0)) #f)
           (list* 0.125 '((G . 0) (B . 0) (D . 0)) #f)
           (list* 0.250 '((A . 0) (C . 0) (D . 0)) #f))
          ;; 8
          (list
           (list* 0.750 '((G . 0) (B . 0) (G . 0)) #f))))
       (define phrase2
         (list
          ;; 1
          (list
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f))
          ;; 2
          (list
           (list* 0.250 '((E . 0) (C . 0) (G . 0)) #f)
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f))
          ;; 3
          (list
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f)
           (list* 0.250 '((D . 0) (B . 0) (G . 0)) #f))
          ;; 4
          (list
           (list* 0.250 '((E . 0) (C . 0) (G . 0)) #f)
           (list* 0.500 '((D . 0) (B . 0) (G . 0)) #f))
          ;; 5
          (list
           (list* 0.250 '((D . 0) (B . 0) (G . 1)) #f)
           (list* 0.250 '((G . 0) (B . 0) (G . 1)) #f)
           (list* 0.250 '((B . 0) (D . 0) (G . 1)) #f))
          ;; 6
          (list
           (list* 0.250 '((D . 1) (D . 0) (G . 1)) #f)
           (list* 0.250 '((B . 0) (D . 0) (G . 1)) #f)
           (list* 0.250 '((G . 0) (D . 0) (G . 0)) #f))
          ;; 7
          (list
           (list* 0.125 '((F# . 0) (D . 0) (A . 0)) #f)
           (list* 0.375 '((A . 0) (C# . 0) (A . 0)) #f)
           (list* 0.250 '((C# . 0) (A . 0) (A . 0)) #f))
          ;; 8
          (list
           (list* 0.750 '((D . 0) (A . 0) (D . 0)) #f))))
       (append phrase1
               phrase1
               phrase2
               phrase1)))))

(require "../../bithoven.rkt")

(define (force-lazy-scale/tones scale tones)
  (for/list ([t*o (in-list tones)])
    (match-define (cons off t-oct) t*o)
    (match-define (cons tone s-oct) (list-ref/modify modify/octave scale off))
    (cons tone (fx+ t-oct s-oct))))
(define (force-lazy-scale/measures scale ms)
  (for/list ([ns (in-list ms)])
    (for/list ([n (in-list ns)])
      (match-define (list* note tones more) n)
      (list* note (force-lazy-scale/tones scale tones) more))))

(define (composition->track c)
  (define scale-kind 
    scale-diatonic-major
    #;
    (select-from-list scales))
  (define scale-root (select-from-list tone-names))
  (define scale (scale-kind scale-root))

  (let ()
    (local-require racket/pretty)
    (pretty-print c))
  (match-define (vector ts pattern parts) c)
  (let ()
    ;; xxx choose this (and make sure every instrument can play the notes)
    (define base-octave (+ 2 (random 2)))
    ;; xxx select this
    (define tempo
      (random 500)
      #;
      (select-from-list
      '(500 360 300 180 170 160 140 135 118 120 115 80 200 280 45 350)))
    (printf "Tempo is ~v\n" tempo)
    (chorded-song->commands
     #:me
     (cons 0.25 tempo)
     #:ts ts
     #:drum
     ;; xxx generate this
     (if (< 80 tempo 170)
         (i:drum (vector (select-from-list (list closed-hihat
                                                 open-hihat
                                                 loose-hihat))
                         bass-drum
                         (select-from-list (list snare-drum1
                                                 snare-drum2))))
         (i:drum (vector #f #f #f)))
     #:drum-measure
     ;; xxx generate this
     (select-from-list
      (list
       ;; simple
       (list (cons 0.125 0)
             (cons 0.125 0)
             (cons 0.125 1)
             (cons 0.125 0)
             (cons 0.125 0)
             (cons 0.125 0)
             (cons 0.125 2)
             (cons 0.125 0))
       ;; straight blues/rock groove
       (list (cons 0.125 1)
             (cons 0.125 0)
             (cons 0.125 2)
             (cons 0.125 0)
             (cons 0.125 1)
             (cons 0.125 0)
             (cons 0.125 2)
             (cons 0.125 0))
       ;; duple drum pattern with triplets
       (list (cons 0.125 1)
             (cons 0.0625 0)
             (cons 0.0625 0)
             (cons 0.125 2)
             (cons 0.0625 0)
             (cons 0.0625 0)
             (cons 0.125 1)
             (cons 0.0625 0)
             (cons 0.0625 0)
             (cons 0.125 2)
             (cons 0.0625 0)
             (cons 0.0625 0))
       ;; with fill
       (list (cons 0.125 1)
             (cons 0.125 0)
             (cons 0.125 2)
             (cons 0.125 0)
             (cons 0.125 1)
             (cons 0.125 0)
             (cons 0.125 2)
             (cons 0.0625 2)
             (cons 0.0625 2))
       ;; without fill
          (list (cons 0.125 1)
                (cons 0.125 0)
                (cons 0.125 2)
                (cons 0.125 0)
                (cons 0.125 1)
                (cons 0.125 1)
                (cons 0.125 2)
                (cons 0.125 0))
          ;; double time
             (list (cons 0.0625 1)
                   (cons 0.0625 0)
                   (cons 0.0625 2)
                   (cons 0.0625 0)
                   (cons 0.0625 1)
                   (cons 0.0625 0)
                   (cons 0.0625 2)
                   (cons 0.0625 0)
                   (cons 0.0625 1)
                   (cons 0.0625 0)
                   (cons 0.0625 2)
                   (cons 0.0625 0)
                   (cons 0.0625 1)
                   (cons 0.0625 0)
                   (cons 0.0625 2)
                   (cons 0.0625 0))
             ;; blast beat
                (list (cons 0.125 1)
                      (cons 0.125 2)
                      (cons 0.125 1)
                      (cons 0.125 2)
                      (cons 0.125 1)
                      (cons 0.125 2)
                      (cons 0.125 1)
                      (cons 0.125 2))
                ;; funk beat / delayed backbeat
                   (list (cons 0.125 1)
                         (cons 0.125 0)
                         (cons 0.125 2)
                         (cons 0.125 0)
                         (cons 0.125 1)
                         (cons 0.125 1)
                         (cons 0.125 0)
                         (cons 0.125 2))
                   ;; heavy metal gallop
                   (list (cons 0.125 1)
                         (cons 0.0625 1)
                         (cons 0.0625 1)
                         (cons 0.125 2)
                         (cons 0.0625 1)
                         (cons 0.0625 1)
                         (cons 0.125 1)
                         (cons 0.0625 1)
                         (cons 0.0625 1)
                         (cons 0.125 2)
                         (cons 0.0625 1)
                         (cons 0.0625 1))
                   ))
     #:instruments
     ;; xxx generate this
     (vector (cons (if (zero? (random 2))
                       (i:pulse (+ 1 (random 2)) 6)
                       (i:pulse-plucky 0.25 (+ 1 (random 2)) 6))
                   (fx+ base-octave 1))
             (cons (if (zero? (random 2))
                       (i:pulse (+ 1 (random 2)) 6)
                       (i:pulse-slow-mod 16 (+ 1 (random 2)) 6))
                   base-octave)
             (cons (i:triangle) (fx- base-octave 2)))
     #:measures
     (append*
      (for/list ([p (in-list pattern)])
        (force-lazy-scale/measures scale (hash-ref parts p)))))))

(module+ main
  (play-one! (cmd:repeat (composition->track (bithoven)))))

(define main-track
  (composition->track (bithoven))
  #;
  (cmd:repeat
   (composition->track (bithoven))
   #;237:Do-What-Is-Right
   #;libbys-song))

(provide main-track)
