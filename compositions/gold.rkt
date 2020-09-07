#lang racket
(require "../music.rkt" rsound (for-syntax syntax/parse racket))

(define-music motif1 (-- [.5 (! 0)] [.5 (! 1)]))
(define-music motif2 (-- [.25 (! 0)] [.25 (! 1)] [.25 (! 2)] [1 (! 3)]))
;; based version
#;(define-music motif1 (-- [.25 (! 0)] [.25 (! 1)] [.25 (! 0)] [.25 (! 1)]))
#;(define-music motif2 (-- [.1 (! 0)] [.1 (! 1)] [.3 (! 3)] [.25 (! 2)] [1 (! 3)]))

(define-realizer over
  (syntax-parser
    [(_ theme:id (p {~optional a #:defaults ([a #'0])} o) ...)
     #'(do [(seq (note p a o) ...)] [theme])]))

;; Dance of Gold by Michiru Yamane
(define-music a/melody
  (in [0 7]
    (in [0 1] (over motif1 [c 4] [f 4]))
    (in [1.75 3.5] (over motif2 [e 4] [f 4] [g 4] [a -1 4]))
    (in [2.5 3.5] (over motif1 [a -1 4] [f 4]))
    (in [3.75 5.5] (over motif2 [e -1 4] [d -1 4] [c 4] [f 4]))
    (in [4.5 5.5] (over motif1 [f 4] [a -1 4]) (in [.25 .5] (note g 4)))
    (in [6 7] (over motif1 [b -1 4] [c 5]))))

(define-music a/chord
  (in [0 8]
    (in [1 2] (over motif1 [a -1 3] [b -1 3]))
    (in [1 2] (over motif1 [c 4]    [d -1 4]))
    (in [3 4] (over motif1 [a -1 3] [g -1 3]))
    (in [3 4] (over motif1 [c 4] [b -1 3]))
    (in [5 5.5] (note a -1 3) (note c 4))
    (in [6 6.5] (note g 3) (note b -1 3) (note e -1 4))
    (in [7 8] (over motif1 [c 4] [e -1 4]))
    (in [7 8] (over motif1 [e -1 4] [f 1 4]))))

(define-music a/bass
  (in [0 8.5]
    (in [0 6] (loop 2 (in [0 1] (over motif1 [c 3] [f 3]))))
    (in [5.5 6] (note e -1 3))
    (in [6.5 7.5] (note a -1 3))
    (in [7.5 8.5] (note a 3))))

(define-music b/melody
   (in [0 8.5]
     (in [0 1] (over motif1 [c 5] [d -1 5]))
     (in [1.75 3.5] (over motif2 [b -1 4] [a -1 4] [g 4] [c 5]))
     (in [3.75 5.5] (over motif2 [f 4] [g 4] [a -1 4] [b -1 4]))
     (in [5.75 7.5] (over motif2 [a -1 4] [g 4] [f 4] [a -1 4]))
     (in [6.5 8.5] (do* [(in [0 1] (seq (note a -1 4) (note g 4)))] [motif1] [(time-scale 2)]))))

(define-music b/cmelody
  (in [0 9]
    (in [1 2] (over motif1 [a -1 5] [g 5]))
    (in [3 4] (over motif1 [g 5] [f 5]))
    (in [5 7] (do* [(in [0 1] (seq (note f 5) (note f 5)))] [motif1] [(time-scale 2)]))
    (in [7 9] (do* [(in [0 1] (seq (note c 5) (note c 5)))] [motif1] [(time-scale 2)]))))

(define-music b/accomp
  (in [0 8.5]
    (in [.5 1.5] (note b -1 3) (note d -1 4) (note f 4))
    (in [1.5 2.5] (note e -1 3) (note g 3) (note b -1 3) (note e -1 4))
    (in [2.5 3.5] (note a -1 3) (note c 4) (note e -1 4))
    (in [3.5 4.5] (note d -1 3) (note f 3) (note a -1 3) (note d -1 4))
    (in [4.5 5.5] (note d 3) (note f 3) (note b -1 3))
    (in [5.5 6.5] (note g 3) (note b 3) (note d 4))
    (in [6.5 7.5] (note c 3) (note f 3) (note c 4))
    (in [7.5 8.5] (note c 3) (note e 3) (note c 4))))

(define-music a/all (in [0 8.5] a/melody a/chord a/bass))
(define-music b/all (in [0 9] b/melody b/cmelody b/accomp))

(play (perform (music (in [0 17] (in [0 8.5] a/all) (in [8 17] b/all)))))
