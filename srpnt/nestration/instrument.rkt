#lang racket/base
(require racket/match
         racket/fixnum
         racket/flonum
         racket/math
         srpnt/music)
(module+ test
  (require rackunit))

;; A traditional ADSR is based on four phases of control:
;; - Attack: The level increases after the ON event for a time
;; - Delay: The level decrease after Attack is over
;; - Sustain: The level stays the same until OFF event
;; - Release: The level drops after the OFF event
;;
;; In my tracker, the amount of time an instrument is played is not
;; expressed as ON/OFF events, but we are called with an explicit time
;; (in frames). This means we don't really need to specify the sustain
;; parameter because we can compute it by subtracting the other
;; parameters.
;;
;; It would be natural to express these times as %s of the input frame
;; time, but in a real instrument when you hold a button on a piano
;; longer it doesn't make the attack longer, instead the attack is a
;; fixed amount of time, like 5ms.
;;
;; The problem then is that if I take explicit times, then if the
;; tempo is so fast that there would be no sustain phase, we want to
;; change it into a percentage. So, I will require the sustain
;; parameter to be specified to compute the expected total time.
;;
;; Then, rather than actual compute the parameter, we return whether
;; the process is in stage 0, 1, 2, or 3 and what percentage through
;; it you are. Further, we will allow the user to specify which phase
;; will fill extra space. [XXX Perhaps I should allow any combination
;; of expanders?]
(define (adsr expander attack decay sustain release)
  (define base (fx+ (fx+ attack decay) (fx+ sustain release)))
  (define base.0 (fx->fl base))
  (define-syntax-rule (define% attack% attack)
    (define attack% (fl/ (fx->fl attack) base.0)))
  (define% attack% attack)
  (define% decay% decay)
  (define% sustain% sustain)
  (define% release% release)
  (λ (total-frames)
    (define total-frames.0 (fx->fl total-frames))
    (define-syntax-rule (maybe-expand sustain attack decay release)
      (if (eq? expander 'sustain)
          (fx->fl (fx- total-frames (fx+ attack (fx+ decay release))))
          (fx->fl sustain)))
    (define-values (attack-len.0 decay-len.0 sustain-len.0 release-len.0)
      (if (fx< total-frames base)
          (values (fl* attack% total-frames.0)
                  (fl* decay% total-frames.0)
                  (fl* sustain% total-frames.0)
                  (fl* release% total-frames.0))
          (values (maybe-expand attack decay sustain release)
                  (maybe-expand decay attack sustain release)
                  (maybe-expand sustain attack decay release)
                  (maybe-expand release attack decay sustain))))
    (define attack-end.0 (fl+ 0.0 attack-len.0))
    (define decay-end.0 (fl+ attack-end.0 decay-len.0))
    (define sustain-end.0 (fl+ decay-end.0 sustain-len.0))
    (define release-end.0 (fl+ sustain-end.0 release-len.0))
    (λ (frame-i)
      (define frame-i.0 (fx->fl frame-i))
      (cond
       [(fl< frame-i.0 attack-end.0)
        (values 0 (fl/ frame-i.0 attack-len.0))]
       [(fl< frame-i.0 decay-end.0)
        (values 1 (fl/ (fl- frame-i.0 attack-end.0) decay-len.0))]
       [(fl< frame-i.0 sustain-end.0)
        (values 2 (fl/ (fl- frame-i.0 decay-end.0) sustain-len.0))]
       [else
        (values 3 (fl/ (fl- frame-i.0 sustain-end.0) release-len.0))]))))

;; A linear interpolaction is ideal for traditional ADSR which is
;; based on slopes
(define (linear lo hi %.0)
  (define lo.0 (fx->fl lo))
  (define hi.0 (fx->fl hi))
  (fl->fx
   (flround
    (fl+ (fl* (fl- 1.0 %.0) lo.0)
         (fl* %.0 hi.0)))))

;; Vibrato, tremolo, as well as duty cycle modulation in a similar
;; framework are different oscillating effects

(define (modulate freq.0 base extent %.0)
  (define freq-split.0 (fl/ freq.0 60.0))
  (define sx (flsin (fl* freq-split.0 (fl* %.0 (fl* 2.0 pi)))))
  (define diff (fl->fx (flround (fl* (fx->fl extent) sx))))
  (fx+ base diff))

(module+ test
  (require plot)
  (plot-new-window? #t)
  (when #f
    (define (plot-one x-max)
      (define borders (vector '(0 . 10) '(10 . 5) '(5 . 5) '(5 . 0)))
      (define phase-f ((adsr 'sustain 5 5 10 5) x-max))
      (define (f x)
        (with-handlers ([exn:fail? (λ (x) (displayln (exn-message x)))])
          (define-values (which %) (phase-f (round x)))
          (match-define (cons lo hi) (vector-ref borders which))
          (define v (linear lo hi %))
          (if (= which 2)
              (modulate 440.0 v 2 %)
              v)))
      (function f #:color x-max #:label (format "frames = ~a" x-max)))
    (plot (list #;(plot-one 10)
           #;(plot-one 25)
           (plot-one 50))
          #:x-min 0 #:x-max 50
          #:y-min 0)))

(struct spec ())
(struct spec:constant spec (v))
(struct spec:adsr spec (expander a as d ds s ss r rs))
(struct spec:% spec (ns))
(struct spec:linear spec (lo hi))
(struct spec:apply spec (f ns))
(struct spec:bind spec (ns f))
(struct spec:modulate spec (freq base extent))
(struct staged ())
(struct staged:adsr spec (fun map))
(define (stage-spec s f)
  (match s
    [(spec:adsr e a as d ds s ss r rs)
     (staged:adsr ((adsr e a d s r) f) (vector as ds ss rs))]
    [(spec:% ns)
     (spec:apply (λ (n) (fl/ (fx->fl n) (fx->fl f))) ns)]
    [_ s]))
(define (eval-spec s f)
  (match s
    [(staged:adsr adsr map)
     (define-values (which %) (adsr f))
     (eval-spec (vector-ref map which) %)]
    [(spec:constant v)
     v]
    [(spec:apply fun ns)
     (eval-spec ns (fun f))]
    [(spec:bind ns fun)
     (fun (eval-spec ns f))]
    [(spec:modulate freq base extent)
     (modulate freq base extent f)]
    [(spec:linear lo hi)
     (linear lo hi f)]))

(define (i:pulse/spec #:duty dspec #:period pspec #:volume vspec)
  (λ (frames tone*accent?)
    (define d* (stage-spec dspec frames))
    (define p* (stage-spec pspec frames))
    (define v* (stage-spec vspec frames))
    (match-define (cons tone accent?) tone*accent?)
    (define base-per (pulse-tone->period tone))
    (define base-volume (if accent? 1 0))
    (for/list ([f (in-range frames)])
      (define duty (fxmin 3 (fxmax 0 (eval-spec d* f))))
      (define per (fx+ base-per (eval-spec p* f)))
      (define volume (fxmin 15 (fxmax 0 (fx+ base-volume (eval-spec v* f)))))
      (wave:pulse duty per volume))))

(define (i:triangle/spec #:on? on?spec #:period pspec)
  (λ (frames tone*accent?)
    (define on?* (stage-spec on?spec frames))
    (define p* (stage-spec pspec frames))
    (match-define (cons tone accent?) tone*accent?)
    (define base-per (triangle-tone->period tone))
    (for/list ([f (in-range frames)])
      (define per (fx+ base-per (eval-spec p* f)))
      (define on? (eval-spec on?* f))
      (wave:triangle on? per))))

(define (i:drum/spec #:mode mspec #:period pspec #:volume vspec)
  (λ (frames)
    (define m* (stage-spec mspec frames))
    (define p* (stage-spec pspec frames))
    (define v* (stage-spec vspec frames))
    (for/list ([f (in-range frames)])
      (define short? (eval-spec m* f))
      (define per (eval-spec p* f))
      (define volume (fxmin 15 (fxmax 0 (eval-spec v* f))))
      (wave:noise short? per volume))))

(define (i:drums drums)
  (λ (frames which-n)
    ((vector-ref drums which-n) frames)))

(provide (all-defined-out))
