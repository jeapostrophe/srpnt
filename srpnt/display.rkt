#lang at-exp racket/base
(require data/enumerate
         data/enumerate/lib
         racket/match
         racket/promise
         racket/system
         racket/format
         racket/list
         srpnt/music-theory
         srpnt/bithoven)

(define (display-bithoven n pth)
  (define b/e (force p:bithoven/e))
  (define bn (if n (modulo n (enum-count b/e)) (random-index b/e)))
  (define b-output (from-nat b/e bn))
  (define comp (bithoven->composition b-output))
  (display-composition bn comp pth)
  comp)

(define (display-composition n c output.pdf)
  (match-define (vector ts _ pattern parts) c)
  (define output.ly (path-replace-suffix output.pdf #".ly"))
  (define render-tones (scale-diatonic-major 'C))
  (define TIME
    (match ts
      ['(4 . 0.25) "4/4"]))
  (define PATTERN
    (apply ~a (add-between pattern " ")))
  (define (NOTE note)
    (match note
      [0.5 "2"]
      [0.25 "4"]
      [0.125 "8"]))
  (with-output-to-file output.ly
    #:exists 'replace
    (λ ()
      (display @~a{
\version "2.18.2"

\header {
  title = \markup { \abs-fontsize #2 {"@n"} }
  subtitle = "@PATTERN"
  composer = "Bithoven"
}

})

      (define seen? (make-hasheq))
      (for ([p (in-list pattern)]
            #:unless (hash-ref seen? p #f))
        (hash-set! seen? p #t)
        (define measures (hash-ref parts p))
        (define (notes doct3 i)
          (apply ~a
           (for*/list ([m (in-list measures)]
                       [n (in-list m)])
             (match-define (list* note (cons tones meta) ac) n)
             (match-define (vector chord-kind chord) meta)
             (match-define (cons idx doct1) (list-ref tones i))
             (match-define (cons name doct2) (list-ref render-tones idx))
             (define doct (+ doct1 doct2 doct3))
             (define DOCT
               (make-string (abs doct)
                            (if (positive? doct) #\' #\,)))
             
             (~a (string-downcase (symbol->string name)) DOCT (NOTE note) " "))))
        (define (chords)
          (define last #f)
          (apply ~a
           (for*/list ([m (in-list measures)]
                       [n (in-list m)])
             (match-define (list* note (cons tones meta) ac) n)
             (match-define (vector chord-kind chord) meta)
             (define KIND
               (match chord-kind
                 ['triad ""]
                 ['seventh ":maj7"]
                 ['sixth ":6"]))
             (match-define (cons name _) (list-ref render-tones chord))
             (define NAME
               (if (eq? last name)
                   "s"
                   (string-downcase (symbol->string name))))
             ;; XXX I use sevenths and sixths that don't exist in
             ;; normal notation and Lilypond can't display these, it
             ;; just errors, so I'm turning that off.
             (begin0 (~a NAME (NOTE note) " ")
               (set! last name)))))
        (define CHORDS (chords))
        (define MELODY (notes +2 0))
        (define HARMONY (notes +1 1))
        (define TENOR (notes 0 2))
        (define BASS (notes -1 3))
        (display @~a{

\score {
<<
  \new ChordNames { @CHORDS }        
  \new Staff { \clef "treble" \key c \major \time @TIME \tempo 4 = 120
<<
  \new Voice = "first"
    { \voiceOne @MELODY }
  \new Voice= "second"
    { \voiceTwo @HARMONY }
>>
  }
  \new Staff { \clef "bass" \key c \major
<<
  \new Voice = "first"
    { \voiceOne @TENOR }
  \new Voice= "second"
    { \voiceTwo @BASS }
>>               
  }
  >>
\header { piece = "@p" }                             
}
                     
}))

      ))
  (system* (find-executable-path "lilypond") output.ly))

(define (maybe-play play? comp)
  (local-require srpnt/player
                 srpnt/band
                 srpnt/nestration/instruments)
  (when play?
    (match-define (vector _ _ _ parts) comp)
    (define strat
      (vector 'C scale-diatonic-major 120
              (i:pulse:plucky 2) (i:pulse:natural 2)
              i:triangle:plucky i:triangle:basic
              i:drums:basic
              (list 0 1 2 3) 2 1 1 1
              (for/hasheq ([p (in-hash-keys parts)])
                (values p beat:straight-rock))
              (for/hasheq ([p (in-hash-keys parts)])
                (values p (cons #f empty)))))
    (let loop ()
      (play-one! (compile-song comp strat))
      (loop))))

(module+ main
  (require racket/cmdline)
  (define the-n #f)
  (define play? #f)
  (command-line
   #:program "display"
   #:once-each
   ["--play" "play it as well" (set! play? #t)]
   ["-n" n "composition number" (set! the-n n)]
   #:args (output-path)
   (maybe-play play? (display-bithoven the-n output-path))))
