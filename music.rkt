#lang racket

(require (for-syntax syntax/parse fancy-app match-plus racket/dict racket/match syntax/stx)
         syntax/parse fancy-app match-plus rsound)
(provide coord tone/s rest/s
         translate score-length
         merge1 merge
         (for-syntax realize1 realize render1 render)
         perform1 perform
         FPS ->frames)

(struct tone/s [pitch] #:transparent)
(struct rest/s [] #:transparent)

(module multi racket

  (require fancy-app match-plus)
  (provide score-length translate merge merge1 coord within?)

  (struct coord [start end] #:transparent)

  (define/match* (within? (coord s1 e1) (coord s2 e2))
    (and (>= s1 s2) (<= e1 e2) (not (= s1 e2))))
  
  (define (score-length comp)
    (for/fold ([m1n +inf.0] [m4x -inf.0] #:result (- m4x m1n))
              ([k (in-dict-keys comp)])
      (match k [(coord s e) (values (min s m1n) (max e m4x))])))

  (define (translate comp t)
    (for/fold ([acc '()])
              ([(k v) (in-dict comp)])
      (match k
        [(coord start end)
         (foldr (merge1 (coord (+ start t) (+ end t)) _ _) acc v)])))
  
  (define (merge1 k v acc) (dict-update acc k (cons v _) '()))
  
  (define (merge comp1 comp2)
    (for*/fold ([acc comp2])
               ([(k v) (in-dict comp1)] [e v])
      (merge1 k e acc))))

(require 'multi (for-syntax (except-in 'multi coord) (rename-in 'multi [coord scoord])))

(define-for-syntax (realize1 c0ord expr env) 
  (define/match* (flat (scoord start end) expr)
    (syntax-parse expr
      [({~datum in} [off-startp:number off-endp:number] exprs ...)
       (define end* (+ start (syntax->datum #'off-endp)))
       (when (> end* end)
         (raise-syntax-error
          'realize
          (format "inner coordinate ends after outer. Max: ~s, Received: ~s" end end*)
          expr))
       (foldr merge '() (map (flat (scoord (+ start (syntax->datum #'off-startp)) end*) _)
                             (syntax->list #'(exprs ...))))]
      [_ (list (list (scoord start end) expr))]))
  
  (define/match* (realize1 (scoord start end) expr)
    (syntax-parse expr 
      [({~datum loop} exprs ...)
       (define flattened
         (foldr merge '() (map (flat (scoord start end) _)
                               (syntax->list #'(exprs ...)))))
       (define length (score-length flattened))
       (realize
        ;; TODO improve
        (for/fold ([acc '()] [prev (translate flattened (- length))] #:result acc)
                  ([i (in-range (floor (/ (- end start) length)))])
          (define new (translate prev length))
          (values (merge new acc) new))
        env)]
      [({~datum offset} n)
       (for/fold ([acc '()])
                 ([(k v) (in-dict env)] #:when (within? k (scoord start end)))
         (define tones (filter (compose (eq? _'tone) car syntax->datum) v))
         (cons (cons k (map (syntax-parser [({~datum tone} expr*) #'(tone (+ expr* n))]) tones))
               acc))]
      [_ (list (list (scoord start end) expr))]))
  
  (define flattened (flat c0ord expr))
  (for*/fold ([acc '()])
             ([(k v) (in-dict flattened)] [e v])
    (merge (realize1 k e) acc)))

(define-for-syntax (realize comp env)
  (for*/fold ([acc '()])
             ([(k v) (in-dict comp)] [e v])
    (merge (realize1 k e env) acc)))

(define-for-syntax (render1 expr _)
  (syntax-parse expr
    [({~datum tone} e) #'(tone/s e)]
    [({~datum quo} e) #'(quo/s e)]
    [{~datum rest} #'(rest/s)]))

(define-for-syntax (render comp env)
  (for/fold ([acc '()] #:result #`(list #,@acc))
            ([(k v) (in-dict comp)])
    (match k
      [(scoord s e)
       (cons #`(cons (coord #,s #,e) (list #,@(stx-map (render1 _ env) v))) acc)])))

(define FPS 44100)
(define ->frames (compose round (* FPS _)))

(define/match* (perform1 (coord (app ->frames start) (app ->frames end)) expr env sound)
  (match expr
    [(tone/s freq)
     (rs-overlay ((if (> start 0) (curry rs-append (silence start)) identity)
                  (make-tone freq .2 (- end start)))
                 sound)]
    [rest/s
     (rs-overlay ((if (> start 0) (curry rs-append (silence start)) identity)
                  (silence (- end start)))
                 sound)]
    [_ sound]))

(define (perform comp env)
  (for*/fold ([acc (silence 1)])
             ([(k v) (in-dict comp)] [e v])
    (perform1 k e env acc)))

(define-syntax with-layer
  (syntax-parser
    [(_ name [exprs ...] rest ...)
     (with-syntax ([(exprs2 ...) (syntax-local-value #'name (λ () #'()))])
       (println #'(exprs2 ...))
       #'(let-syntax ([name #'((exprs ...) exprs2 ...)])
           rest ...))]))

(define-syntax assemble
  (syntax-parser
    [(_ name [start:number end:number])
     (define comp (reverse (syntax->list (syntax-local-value #'name (λ () #'())))))
     (println comp)
     (render
      (let loop ([syntax-env '()] [layers comp])
        (match layers
          [(cons layer layers)
           (define l (realize (list (cons (scoord (syntax->datum #'start)
                                                  (syntax->datum #'end))
                                          (syntax->list layer)))
                              syntax-env))
           (define l* (dict-map l (λ (k v) (cons k v))))
           (merge l* (loop (merge l syntax-env) layers))]
          [_ '()]))
      '())]))

(define (bach a b c d e)
  (with-layer snip
    [(loop (in [0 .5] (tone a))
           (in [.5 1] (tone b))
           (in [1 1.5] (tone c))
           (in [1.5 2] (tone d))
           (in [2 2.5] (tone e))
           (in [2.5 3] (tone c))
           (in [3 3.5] (tone d))
           (in [3.5 4] (tone e)))]
         (with-layer snip
           [(offset -210)]
           (assemble snip [0 8]))))

(play (perform (bach 440 550 660 880 1100) '()))
