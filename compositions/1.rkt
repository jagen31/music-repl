#lang racket

(require "../music.rkt" rsound)

(define-music snip
  (in [0 4]
      (in [0 .5] (tone (! 0)))
      (in [.5 1] (tone (! 1)))
      (in [1 1.5] (tone (! 2)))
      (in [1.5 2] (tone (! 3)))
      (in [2 2.5] (tone (! 4)))
      (in [2.5 3] (tone (! 2)))
      (in [3 3.5] (tone (! 3)))
      (in [3.5 4] (tone (! 4)))))

(define-values (B C D E F G A B+ C+ D+ E+ F+)
  (values 246.94 261.63 293.66 329.63 349.23 392 440 493.88 523.25 587.33 659.26 698.46))

(play
 (perform
  (music
   (in [0 32]
     (do [(-- [8 (seq C E G C+ E+)]
              [8 (seq C D A D+ F+)]
              [8 (seq B D G D+ F+)]
              [8 (seq C E G C+ E+)])]
         [(loop 4 snip)])))))
