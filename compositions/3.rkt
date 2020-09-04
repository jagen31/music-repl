#lang racket
(require "../music.rkt" rsound (for-syntax syntax/parse racket))

(define-music motif1 [(in [0 .5] (! 0)) (in [.5 1] (! 1))])
(define-music motif2
    [(in [0 .25] (! 0)) (in [.25 .5] (! 1)) (in [.5 .75] (! 2)) (in [.75 1.75] (! 3))])
;; based version
#;(define-music motif1 [(in [0 .25] (! 0)) (in [.25 .5] (! 1)) (in [.5 .75] (! 0)) (in [.75 1] (! 1))])
#;(define-music motif2
    [(in [0 .1] (! 0)) (in [.1 .2] (! 1)) (in [.2 .5] (! 3)) (in [.5 .75] (! 2)) (in [.75 1.75] (! 3))])

(define-music motif1-scaled
  [(music motif1)]
  [(time-scale 2)])

(define-realizer over
  (syntax-parser
    [(_ theme:id (p {~optional a #:defaults ([a #'0])} o) ...)
     #'(music [(seq (note p a o) ...)] [(music theme)])]))

(define-realizer over*
  (syntax-parser
    [(_ theme:id (p {~optional a #:defaults ([a #'0])} o) ...)
     #'(music* [(seq (note p a o) ...)] [(music* theme)])]))

;; Dance of Gold by Michiru Yamane
(define-music melody/a
  [(in [0 1] (over motif1 [c 4] [f 4]))
   (in [1.75 3.5] (over motif2 [e 4] [f 4] [g 4] [a -1 4]))
   (in [2.5 3.5] (over motif1 [a -1 4] [f 4]))
   (in [3.75 5.5] (over motif2 [e -1 4] [d -1 4] [c 4] [f 4]))
   (in [4.5 5.5] (over motif1 [f 4] [a -1 4]) (in [.25 .5] (note g 4)))
   (in [6 7] (over motif1 [b -1 4] [c 5]))])

(define-music chord/a
  [(in [1 2] (over motif1 [a -1 3] [b -1 3]))
   (in [1 2] (over motif1 [c 4]    [d -1 4]))
   (in [3 4] (over motif1 [a -1 3] [g -1 3]))
   (in [3 4] (over motif1 [c 4] [b -1 3]))
   (in [5 5.5] (note a -1 3) (note c 4))
   (in [6 6.5] (note g 3) (note b -1 3) (note e -1 4))
   (in [7 8] (over motif1 [c 4] [e -1 4]))
   (in [7 8] (over motif1 [e -1 4] [f 1 4]))])

(define-music melody/b
  [(in [0 1] (over motif1 [c 5] [d -1 5]))
   (in [1.75 3.5] (over motif2 [b -1 4] [a -1 4] [g 4] [c 5]))
   (in [3.75 5.5] (over motif2 [f 4] [g 4] [a -1 4] [b -1 4]))
   (in [5.75 7.5] (over motif2 [a -1 4] [g 4] [f 4] [a -1 4]))
   (in [6.5 8.5] (over* motif1-scaled [a -1 4] [g 4]))])

(define-music cmelody/b
  [(in [1 2] (over motif1 [a -1 5] [g 5]))
   (in [3 4] (over motif1 [g 5] [f 5]))
   (in [5 6] (note f 5))
   (in [6 7] (note f 5))
   (in [7 8] (note c 5))
   (in [8 8.5] (note c 5))])

(define-music accomp/b
  [(in [.5 1.5] (note b -1 3) (note d -1 4) (note f 4))
   (in [1.5 2.5] (note e -1 3) (note g 3) (note b -1 3) (note e -1 4))
   (in [2.5 3.5] (note a -1 3) (note c 4) (note e -1 4))
   (in [3.5 4.5] (note d -1 3) (note f 3) (note a -1 3) (note d -1 4))
   (in [4.5 5.5] (note d 3) (note f 3) (note b -1 3))
   (in [5.5 6.5] (note g 3) (note b 3) (note d 4))
   (in [6.5 7.5] (note c 3) (note f 3) (note c 4))
   (in [7.5 8.5] (note c 3) (note e 3) (note c 4))])

(define-music bass/a
  [(in [0 6] (loop 2 (in [0 1] (over motif1 [c 3] [f 3]))))
   (in [5.5 6] (note e -1 3))
   (in [6.5 7.5] (note a -1 3))
   (in [7.5 8.5] (note a 3))])

(define-music all/a
  [(in [0 8.5] (music melody/a) (music chord/a) (music bass/a))])

(define-music all/b
  [(in [0 8.5] (music melody/b) (music cmelody/b) (music accomp/b))])

(play (perform (assemble-music [0 16.5]
                               [(in [0 8.5] (music all/a))]
                               [(in [8 16.5] (music all/b))])))
