#lang racket

(require "../music.rkt" rsound rsound/piano-tones rsound/envelope
         (for-syntax syntax/parse syntax/stx racket/match))

(define-realizer note
  (syntax-parser
    [(_ pitch octave) #'(note pitch 0 octave)]
    [(_ p accidental o)
     #'(midi (+ (match 'p ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11]) accidental (* 12 o) 12))]))

(define-performer (midi semitone start end)
  (define len (- end start))
  ((if (> start 0) (curry rs-append (silence start)) identity)
   (rs-mult
    (piano-tone semitone)
    ((adsr 2 1.0 2 1.0 (round (* 1/4 len))) len))))

(define-music accomp
  [(in [0 .75] (! 0))
   (in [.75 1] (! 1))
   (in [1 1.75] (! 2))
   (in [1.75 2] (! 1))])

(define-music motif1
  [(in [0 1] (note c 4))]
  [(in [1 2] (note e 4))]
  [(in [2 4] (note g 4))])

(define-music motif2
  [(in [0 1] (note g 4))]
  [(in [1 2] (note d 4))]
  [(in [2 3] (note f 4))]
  [(in [3 4] (note d 4))])

(define-music a1
  [(in [0 4] (seq (note c 3) (note e 3) (note g 3)))]
  [(in [0 4] (loop 2 (music accomp))) (music motif1)])

(define-music a2
  [(in [0 4] (seq (note b 2) (note d 3) (note g 3)))]
  [(in [0 4] (loop 2 (music accomp))) (music motif2)])

(play (perform
       (assemble-score [0 8]
                       [(music a1)]
                       [(in [4 8] (music a2))])
       '()))
