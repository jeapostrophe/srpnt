#lang racket/base
(require racket/fixnum
         racket/flonum
         racket/list
         racket/port
         racket/match
         srpnt/player
         srpnt/music)
(module+ test
  (require rackunit))

(define (read-variable-length ip)
  (define c (read-byte ip))
  (define more-mask #b10000000)
  (cond
   [(fx= 0 (bitwise-and c more-mask))
    (values 1 c)]
   [else
    (define-values (len more) (read-variable-length ip))
    (values (fx+ 1 len)
            (fx+ more
                 (arithmetic-shift (bitwise-xor c more-mask)
                                   (fx* 7 len))))]))

(define (read-variable-length-num ip)
  (define-values (_ n) (read-variable-length ip))
  n)

(module+ test
  (define (rvl-bs bs)
    (read-variable-length-num (open-input-bytes bs)))
  (check-equal? (rvl-bs (bytes #b00000000))
                #b00000000)
  (check-equal? (rvl-bs (bytes #b10000001 #b01001000))
                #b11001000)
  (check-equal? (rvl-bs (bytes #b11000000 #b10000000 #b00000000))
                #b000100000000000000000000)
  (define-syntax-rule (test-rvl out . in)
    (check-equal? (rvl-bs (bytes . in)) out))
  (define-syntax-rule (test-rvls t ...)
    (begin (test-rvl . t) ...))
  (test-rvls
   (#x00000000    #x00)
   (#x00000040    #x40)
   (#x0000007F    #x7F)
   (#x00000080    #x81 #x00)
   (#x00002000    #xC0 #x00)
   (#x00003FFF    #xFF #x7F)
   (#x00004000    #x81 #x80 #x00)
   (#x00100000    #xC0 #x80 #x00)
   (#x001FFFFF    #xFF #xFF #x7F)
   (#x00200000    #x81 #x80 #x80 #x00)
   (#x08000000    #xC0 #x80 #x80 #x00)
   (#x0FFFFFFF    #xFF #xFF #xFF #x7F)))

(define (read-header ip)
  (define id (read-bytes 4 ip))
  (define size (read-bytes 4 ip))
  (values id (integer-bytes->integer size #f #t)))

(define (read-chunk ip)
  (define-values (id chunk-size) (read-header ip))
  (values id (read-bytes chunk-size ip)))

(define (expect id e a)
  (unless (equal? e a)
    (error id "Expected ~v and got ~v" e a)))

(define (expect-any id es a)
  (unless (member a es)
    (error id "Expected ~v and got ~v" es a)))

(define (read-header-chunk ip)
  (define-values (hid hbs) (read-chunk ip))
  (expect 'read-header-chunk #"MThd" hid)
  (define fmt (integer-bytes->integer hbs #f #t 0 2))
  (define tracks (integer-bytes->integer hbs #f #t 2 4))
  (define time-div (integer-bytes->integer hbs #f #t 4 6))
  (values fmt tracks time-div))

(struct event (delta))
(struct event:meta event (type data))
(struct event:set-tempo event (24ths-of-a-microsecond-per-MIDI-clock))
(struct event:sysex event (data))
(struct event:control-mode-change event (chan data))
(struct event:program-change event (chan program))
(struct event:note-on event (chan num velo))
(struct event:note-off event (chan num velo))

(define (read-meta-event delta ip)
  (define meta-type (read-byte ip))
  (define len (read-variable-length-num ip))
  (define bs (read-bytes len ip))
  (match meta-type
    [#x51
     (event:set-tempo delta
                      (integer-bytes->integer (bytes-append (bytes 0) bs) #f #t))]
    [_
     (printf "META: ~v ~v ~v\n" (hex-byte meta-type) len bs)
     (event:meta delta meta-type bs)]))

(define (read-sysex-event delta ip)
  (define len (read-variable-length-num ip))
  (define bs (read-bytes len ip))
  (printf "SYSEX: ~v ~v\n" len bs)
  (event:sysex delta bs))

(define (hex-byte b)
  (local-require file/sha1)
  (bytes->hex-string (bytes b)))

(define (read-control-data ip)
  (define p (peek-byte ip))
  (define more-len
    (cond
     [(member p '(#x60 #x61 #x7b #x7c #x7D #x7F)) 0]
     [(member p '(#x7e)) (error 'rcd "7E")]
     [(<= #x62 p #x79) 0]
     [else 1]))
  (define bs
    (read-bytes (+ 1 more-len) ip))
  (printf "CD: ~v ~v ~v\n" (hex-byte p) more-len bs)
  bs)

(define (read-note ip)
  (define rn (read-byte ip))
  (define n (bitwise-and #b1111111 rn))
  (unless (<= 0 n 127)
    (error 'read-note "Note isn't in correct range: ~v\n" n))
  n)

(define (read-velocity ip)
  (define rn (read-byte ip))
  (define n (bitwise-and #b1111111 rn))
  (unless (<= 0 n 127)
    (error 'read-velocity "Velocity isn't in correct range: ~v\n" n))
  n)

(define (read-event last-status ip)
  (let/ec esc
    (define (done!)
      (esc last-status eof))
    (when (eof-object? (peek-byte ip))
      (done!))
    (define delta (read-variable-length-num ip))
    (define b (read-byte ip))
    (when (eof-object? b)
      (done!))
    (define (parse-event b)
      (define 1st-part (bitwise-and #xF0 b))
      (match 1st-part
        ;; http://www.onicos.com/staff/iz/formats/midi-event.html
        [#x80
         (define num (read-note ip))
         (define vel (read-velocity ip))
         (values b (event:note-off delta 0 num vel))]
        [#x90
         (define num (read-note ip))
         (define vel (read-velocity ip))
         (values b
                 ((if (fx= 0 vel) event:note-off event:note-on)
                  delta 0 num vel))]
        [#xB0
         (values b (event:control-mode-change delta 0 (read-control-data ip)))]
        [#xC0
         (values b (event:program-change delta 0 (read-byte ip)))]
        [#xF0
         (match b
           [(or #xFF)
            (values b (read-meta-event delta ip))]
           [(or #xF0 #xF7)
            (values b (read-sysex-event delta ip))])]
        [(or #xA0 #xD0 #xE0)
         (error 'read-event "Unimplemented ~v\n" (hex-byte b))]
        [sb
         (parse-event last-status)]))
    (parse-event b)))

(define (read-track ip)
  (define-values (tid tbs) (read-chunk ip))
  (expect 'read-track #"MTrk" tid)
  (define tbip (open-input-bytes tbs 'track))
  (let loop ([last-status #f])
    (define-values (next-status e) (read-event last-status tbip))
    (if (eof-object? e)
        empty
        (cons e (loop next-status)))))

(struct midi (time-div tracks))

(define (read-midi p)
  (call-with-input-file p
    (λ (ip)
      (define-values (fmt tracks time-div) (read-header-chunk ip))
      (expect-any 'read-midi '(0 1) fmt)
      (midi time-div
            (for/list ([t (in-range tracks)])
              (read-track ip))))))

(define (midi-num->freq num)
  (fl* 440.0 (flexpt 2.0 (fl/ (fl- (fx->fl num) 69.0) 12.0))))

(struct note (num velo) #:transparent)
(struct combined (len tempo notes) #:transparent)

(define (remove-note on num)
  (define sil
    (sort (hash-keys on) < #:key (λ (sn) (abs (- sn num)))))
  (if (empty? sil)
      on
      (hash-remove on (first sil))))

(struct midic (time-div cs))

(define (dprintf . args)
  (when #f
    (apply printf args)))

(define (midi-len->frames ticks-per-quarter-note trial-frames-per-quarter-note.0 ticks)
  (define ticks.0 (fx->fl ticks))
  (define ticks-per-quarter-note.0 (fx->fl ticks-per-quarter-note))
  (define quarter-notes-per-tick (fl/ 1.0 ticks-per-quarter-note.0))
  (define quarter-notes.0 (fl* ticks.0 quarter-notes-per-tick))
  (define frames-per-quarter-note.0 30.0)
  (define frames.0 (fl* frames-per-quarter-note.0 quarter-notes.0 ))
  (fl->fx (flround frames.0)))

(define (midi-velo->vol velo)
  (fl->fx (flround (fl* 15.0 (fl/ (fx->fl velo) 127.0)))))

(define (note->pulse p)
  (match p
    [#f #f]
    [(note num velo)
     (define per (pulse-freq->period (midi-num->freq num)))
     (and per (wave:pulse 2 per (midi-velo->vol velo)))]))

(define (note->triangle t)
  (match t
    [#f #f]
    [(note num velo)
     (define per (triangle-freq->period (midi-num->freq num)))
     (and per (wave:triangle #t per))]))

(define (notes->frame on)
  (define notes (hash-keys on))
  (define ordered-n (sort notes <))
  (define-values (t more)
    (match ordered-n
      [(list)
       (values #f ordered-n)]
      [(list* num more)
       (define n (note num (hash-ref on num)))
       (define t (note->triangle n))
       (if t
           (values t more)
           (values #f ordered-n))]))
  (define p-playable
    (filter-map (λ (num) (note->pulse (note num (hash-ref on num)))) more))
  (dprintf "N: ~v\n" (for/list ([num (in-list ordered-n)])
                       (cons num (hash-ref on num))))
  (define-values (p1 p2)
    (match p-playable
      [(list) (values #f #f)]
      [(list p2) (values p2 #f)]
      [(list p2 middle ... p1) (values p2 p1)]))
  (cmd:frame* p1 p2 t
              #f #f #f))

(define (raw-tempo->frames-per-qnote microseconds-per-qnote)
  (define microseconds-per-qnote.0 (fx->fl microseconds-per-qnote))
  (define usecs-per-sec.0 (fl* 1000.0 1000.0))
  (define seconds-per-qnote.0 (fl/ microseconds-per-qnote.0 usecs-per-sec.0))
  (define frames-per-second.0 (fl/ 44100.0 600.0))
  (define frames-per-qnote.0 (fl* seconds-per-qnote.0 frames-per-second.0))
  frames-per-qnote.0)

(define (combine m)
  (match-define (midi time-div tracks) m)
  (define-values (last-len last-tempo last-on last-l)
    ;; tempo is frames-per-quarter-note
    (for/fold ([ldelta 0] [tempo 30.0] [on (hasheq)] [l empty])
              ([e (in-list (last tracks))])
      (match e
        [(event:note-on delta chan num velo)
         (values 0
                 tempo
                 (hash-set on num velo)
                 (cons (combined (fx+ ldelta delta) tempo on) l))]
        [(event:note-off delta chan num velo)
         (values 0
                 tempo
                 (remove-note on num)
                 (cons (combined (fx+ ldelta delta) tempo on) l))]
        [(event:set-tempo delta new-raw-tempo)
         (values 0
                 (raw-tempo->frames-per-qnote new-raw-tempo)
                 on
                 (cons (combined (fx+ ldelta delta) tempo on) l))]
        [(event delta)
         (values (fx+ ldelta delta) tempo on l)])))
  (define rev-cmbs (cons (combined last-len last-tempo last-on) last-l))
  (midic time-div (reverse rev-cmbs)))

(define (convert m)
  (define mc (combine m))
  (match-define (midic time-div cmbs) mc)
  (let ()
    (dprintf "~v\n"
             (vector time-div (bitwise-bit-set? time-div 16)
                     (bitwise-and time-div #x7FFF))))
  (for/list ([c (in-list cmbs)])
    (match c
      [(combined len tempo l)
       (cmd:hold* (midi-len->frames time-div tempo len)
                  (notes->frame l))])))

(define (convert-and-play! midi-p)
  (printf "FILE: ~v\n" midi-p)
  (define m (read-midi midi-p))
  (define c (convert m))
  (dprintf "C: ~e\n" c)
  (play-one! c))

;; I have no idea why this works on some MIDI files and not
;; others. Basically everything on http://www.ninsheetm.us/ works but
;; I haven't found nay classical music that works.
;;
;; I believe the heart of the problem is handling tempo incorrectly. I
;; attempted to do this with midi-len->frames and
;; raw-tempo->frames-per-qnote but they seem to give nonsense
;; answers. I don't really care too much about this, since this was
;; just a demo.
(module+ main
  (require racket/cmdline)
  (command-line #:program "midi"
                #:args (midi-p)
                (convert-and-play! midi-p)))
