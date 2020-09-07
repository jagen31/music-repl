#lang racket
(require "../music.rkt" rsound (for-syntax syntax/parse))

(define-music motif1 (in [0 2] (in [0 .5] (! 0)) (in [.5 1] (! 1)) (in [1 1.5] (! 2)) (in [1.5 2] (! 3))))

(define-music a/melody
  (-- [6 (loop 2
           (in [0 2]
             (do [(seq (note e 4) (note a 4) (note c 5) (note b 4))]
                 [motif1])))]
      [2 (do [(seq (note g 1 4) (note a 4) (note f 4) (note e 4))]
             [motif1])]))

(define-music a/bass
  (-- [6 (loop 2
           (in [0 2]
             (do [(seq (note g 1 3)
                       (note a 3)
                       (in [0 1] (in [0 .25] (note c 4)) (in [.25 .5] (note d 4)))
                       (note e 4))]
                 [motif1])))]
      [2 (do [(seq (note e 3) (note f 3) (note a 3) (note g 1 3))] [motif1])]))

(play (perform (music (in [0 8] a/melody a/bass))))
