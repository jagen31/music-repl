#lang racket

(require syntax/parse fancy-app match-plus rsound)
(provide coord tone
         merge1 merge
         translate score-length
         realize1 realize
         perform1 perform
         FPS ->frames)

(struct coord [start end] #:transparent)
(struct tone [pitch] #:transparent)

(define (merge1 k v acc) (dict-update acc k (cons v _) '()))

(define (merge comp1 comp2)
  (for*/fold ([acc comp2])
             ([(k v) (in-dict comp1)] [e v])
    (merge1 k e acc)))

(define (translate comp t)
  (for/fold ([acc '()])
            ([(k v) (in-dict comp)])
    (match k
      [(coord start end)
       (foldr (merge1 (coord (+ start t) (+ end t)) _ _) acc v)])))

(define (score-length comp)
  (for/fold ([m1n +inf.0] [m4x -inf.0] #:result (- m4x m1n))
            ([k (in-dict-keys comp)])
    (match k [(coord s e) (values (min s m1n) (max e m4x))])))

(define (realize1 c0ord expr env) 
  (define/match* (flat (coord start end) expr)
    (syntax-parse expr
      [({~literal in} [off-startp:number off-endp:number] exprs ...)
       (define end* (+ start (syntax->datum #'off-endp)))
       (when (> end* end)
         (raise-syntax-error
          'realize
          (format "inner coordinate ends after outer. Max: ~s, Received: ~s" end end*)
          expr))
       (foldr merge '() (map (flat (coord (+ start (syntax->datum #'off-startp)) end*) _)
                             (syntax->list #'(exprs ...))))]
      [_ (list (list (coord start end) expr))]))
  
  (define/match* (realize1 (coord start end) expr)
    (syntax-parse expr 
      [({~literal loop} exprs ...)
       (define flattened
         (foldr merge '() (map (flat (coord start end) _)
                               (syntax->list #'(exprs ...)))))
       (define length (score-length flattened))
       (realize
        ;; TODO improve
        (for/fold ([acc '()] [prev (translate flattened (- length))] #:result acc)
                  ([i (in-range (floor (/ (- end start) length)))])
          (define new (translate prev length))
          (values (merge new acc) new))
        env)]
      [_ (list (list (coord start end) expr))]))
  
  (define flattened (flat c0ord expr))
  (for*/fold ([acc '()])
             ([(k v) (in-dict flattened)] [e v])
    (merge (realize1 k e) acc)))

(define (realize comp env)
  (for*/fold ([acc '()])
             ([(k v) (in-dict comp)] [e v])
    (merge (realize1 k e env) acc)))

(define FPS 44100)
(define ->frames (compose round (* FPS _)))

(define/match* (perform1 (coord (app ->frames start) (app ->frames end)) expr sound)
  (syntax-parse expr
    [({~literal tone} p)
     (define freq (eval #'p))
     (rs-overlay ((if (> start 0) (curry rs-append (silence start)) identity)
                  (make-tone freq .2 (- end start)))
                 sound)]
    [{~literal rest}
     (rs-overlay ((if (> start 0) (curry rs-append (silence start)) identity)
                  (silence (- end start)))
                 sound)]
    [_ sound]))

(define (perform comp)
  (for*/fold ([acc (silence 1)])
             ([(k v) (in-dict comp)] [e v])
    (perform1 k e acc)))

