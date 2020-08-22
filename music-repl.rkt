#lang racket

(require (for-syntax syntax/parse "music.rkt") syntax/parse fancy-app match-plus rsound "music.rkt")

(provide (rename-out [ti #%top-interaction]))

(define-for-syntax score (box '()))
(define-for-syntax layer (box '()))

(define rendered '())
(define performance '())

(define current-coord (box (coord 0 4)))

(define frame-ctr
  (network ()
    [out = (let ([prev (prev out 0)])
             (if (>= prev (sub1 (rs-frames (unbox rendered))))
               0
               (add1 prev)))]))

(define-syntax ti
  (syntax-parser
    #:datum-literals (realize clear play loop stop show score layer coord)
    ;; incrementally realize layer
    [(_ . realize)
     (define l (unbox layer))
     (define s (unbox score))
     (define l* (realize l s))
     (set-box! score (merge l* s))
     #`(let ()
         (define r (unbox render))
         (define r* #,(render l* r))
         (set-box! rendered (merge r *))
         (define p (perform r* r))
         (set-box! performed (merge p (unbox performed))))]
    [(_ . clear) #'(set-box! layer '())]
    [(_ . play) #'(begin (play (perform (unbox score))) (void))]
    [(_ . loop)
     #'(void
        (signal-play
         (network ()
                  [c <= frame-ctr]
                  [out = (rs-ith/left (unbox rendered) c)])))]
    [(_ . stop) #'(void (stop))]
    [(_ . ({~seq show coord}))
     #'(match (unbox current-coord)
         [(coord start end) (printf "[~s,~s]\n" start end)])]
    [(_ . ({~seq show {~or* {~and score sc}
                            layer}}))
     #:with dict #`(unbox #,(if (attribute sc) #'score #'layer))
     #'(void (dict-map dict
                       (match-lambda**
                        [((coord s e) v)
                         (printf "(in [~s ~s] ~s)\n" s e (map syntax->datum v))])))]
    [(_ . [start:number end:number])
     #'(set-box! current-coord (coord (syntax->datum #'start) (syntax->datum #'end)))]
    [(_ . expr)
     #'(set-box! layer (merge1 (unbox current-coord) #'expr (unbox layer)))]))
