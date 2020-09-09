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
     #'(do* [(seq (note p a o) ...)] [theme])]))

;; Dance of Gold by Michiru Yamane
(define-music a/melody
  (-- [1.75 (over motif1 [c 4] [f 4])]
      [2 #:elide 1.25 (over motif2 [e 4] [f 4] [g 4] [a -1 4])]
      [1.25 (over motif1 [a -1 4] [f 4])]
      [2 #:elide 1.25 (over motif2 [e -1 4] [d -1 4] [c 4] [f 4])]
      [1.5 (over motif1 [f 4] [a -1 4]) (in [.25 .5] (note g 4))]
      [1 (over motif1 [b -1 4] [c 5])]))

(define-music a/chord
  (-- 1
    [2 (over motif1 [a -1 3] [b -1 3]) (over motif1 [c 4] [d -1 4])]
    [2 (over motif1 [a -1 3] [g -1 3]) (over motif1 [c 4] [b -1 3])]
    [.5 #:overlap -.5 (note a -1 3) (note c 4)]
    [.5 #:overlap -.5 (note g 3) (note b -1 3) (note e -1 4)]
    [1 (over motif1 [c 4] [e -1 4]) (over motif1 [e -1 4] [f 1 4])]))

(define-music a/bass
  (-- [6 #:overlap .5 (loop 2 (in [0 1] (over motif1 [c 3] [f 3])))]
      [.5 #:overlap -.5 (note e -1 3)]
      [1 (note a -1 3)]
      [1 (note a 3)]))

(define-music b/melody
  (-- [1.75 (over motif1 [c 5] [d -1 5])]
      [2 (over motif2 [b -1 4] [a -1 4] [g 4] [c 5])]
      [2 (over motif2 [f 4] [g 4] [a -1 4] [b -1 4])]
      [1.75 #:elide 1 (over motif2 [a -1 4] [g 4] [f 4] [a -1 4])]
      [2 (do* [(in [0 1] (seq (note a -1 4) (note g 4)))] [motif1] [(time-scale 2)])]))

(define-music b/cmelody
  (-- 1
    [2 (over motif1 [a -1 5] [g 5])]
    [2 (over motif1 [g 5] [f 5])]
    [2 (do* [(in [0 1] (seq (note f 5) (note f 5)))] [motif1] [(time-scale 2)])]
    [2 (do* [(in [0 1] (seq (note c 5) (note c 5)))] [motif1] [(time-scale 2)])]))

(define-music b/accomp
  (-- .5
    [1 (note b -1 3) (note d -1 4) (note f 4)]
    [1 (note e -1 3) (note g 3) (note b -1 3) (note e -1 4)]
    [1 (note a -1 3) (note c 4) (note e -1 4)]
    [1 (note d -1 3) (note f 3) (note a -1 3) (note d -1 4)]
    [1 (note d 3) (note f 3) (note b -1 3)]
    [1 (note g 3) (note b 3) (note d 4)]
    [1 (note c 3) (note f 3) (note c 4)]
    [1 (note c 3) (note e 3) (note c 4)]))

(play (perform (music (-- [8.5 #:overlap .5 a/melody a/chord a/bass]
                          [9 b/melody b/cmelody b/accomp]))))
