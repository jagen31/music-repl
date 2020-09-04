#lang racket

(require (for-syntax fancy-app match-plus
                     racket/list racket/set racket/dict racket/match
                     syntax/parse syntax/stx)
         syntax/parse fancy-app match-plus rsound rsound/envelope)
(provide coord tone rest
         translate score-length
         merge1 merge
         (for-syntax realize render)
         perform1 perform
         FPS ->frames)

(struct tone [freq] #:transparent)
(struct rest [] #:transparent)

(module multi racket

  (require fancy-app match-plus)
  (provide tee score-length translate merge merge1 coord within?)
  
  (define (tee v) (println v) v)

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

(define-for-syntax (find-all c0ord parser env)
  (for*/fold ([acc '()])
             ([(k v) (in-dict env)] #:when (within? c0ord k) [e v])
    (define result (parser e))
    (if result (cons result acc) acc)))

(begin-for-syntax
  (define current-coordinate (make-parameter (scoord 0 +inf.0)))
  (define current-env (make-parameter '())))

(define-for-syntax performers (list #'tone #'rest))

(define-for-syntax realizers
  (list
   (cons #'in
         (syntax-parser
           [(_ [off-startp:number off-endp:number] exprs ...)
            (match-define (scoord start end) (current-coordinate))
            (define start* (+ start (syntax->datum #'off-startp)))
            (define end* (+ start (syntax->datum #'off-endp)))
            (when (> end* end)
              (raise-syntax-error 'realize
                                  (format "inner coordinate ends after outer. Max: ~s, Received: ~s" end end*)
                                  #'off-endp))
            #`(comp ([#,start* #,end*] #,@(syntax->list #'(exprs ...))))]))
   
   (cons #'loop
         (syntax-parser
           ;; length is temporary while I figure this out
           [(_ length*:number exprs ...)
            (match-define (scoord start end) (current-coordinate))
            #;(define flattened (unrender (realize #`(comp ([#,start #,end] exprs ...)))))
            (define length (syntax->datum #'length*))
            (realize
             (render
              ;; TODO improve
              (for/fold ([acc '()]
                         [prev (translate (list (cons (scoord start end)
                                                      (syntax->list #'(exprs ...))))
                                          (- length))]
                         #:result acc)
                        ([i (in-range (floor (/ (- end start) length)))])
                (define new (translate prev length))
                (values (merge new acc) new))))]))

   (cons #'offset
         (syntax-parser
           [(_ n)
            (for/fold ([acc '()])
                      ([(k v) (in-dict (current-env))] #:when (within? k (current-coordinate)))
              (define tones (filter (compose (eq? _'tone) car syntax->datum) v))
              (cons (cons k (map (syntax-parser [({~datum tone} expr*) #'(tone (+ expr* n))])
                                 tones))
                    acc))]))

   (cons #'!
         (syntax-parser
           [(_ ix)
            (car (find-all (current-coordinate)
                           (syntax-parser
                             [({~datum seq} exprs ...)
                              (list-ref (syntax->list #'(exprs ...))
                                        (syntax->datum #'ix))]
                             [_ #f])
                           (current-env)))]))
   
   (cons #'music
         (syntax-parser
           [(_ id:id) (render (assemble* (syntax-local-value #'id)))]))))

(define-for-syntax (realize expr)
  (define expr*
    (syntax-parse expr
      [(id:id _ ...)
       (or (for/or ([(k trans) (in-dict realizers)])
             (and (free-identifier=? #'id k)
                  (realize (trans expr))))
           expr)]
      [_ expr]))
  (syntax-parse expr*
    [({~datum comp} clause ...)
     (render
      (for/fold ([acc '()])
                ([clause (syntax->list #'(clause ...))])
        (syntax-parse clause
          [([start end] exprs ...)
           (define (make-clauses expr)
             (syntax-parse expr
               [({~datum comp} clauses ...)
                (for/list ([clause (syntax->list #'(clauses ...))])
                  (syntax-parse clause
                    [([start end] exprs ...)
                     (cons (scoord (syntax->datum #'start) (syntax->datum #'end))
                           (syntax->list #'(exprs ...)))]))]
               [expr (list (cons (scoord (syntax->datum #'start) (syntax->datum #'end))
                                 (list #'expr)))]))
           (parameterize ([current-coordinate (scoord (syntax->datum #'start) (syntax->datum #'end))])
             (foldr merge acc (stx-map (compose make-clauses realize) #'(exprs ...))))])))]
    [(exprs ...) #`(#,@(stx-map realize #'(exprs ...)))]
    [_ expr*]))

(define-syntax comp
  (syntax-parser
    [(_ ([start end] exprs ...) ...)
     #'(list (cons (coord start end) (list exprs ...)) ...)]))

(define-for-syntax (render comp [capture #f])
  (for/fold ([acc '()] #:result #`(comp #,@acc))
            ([(k v) (in-dict comp)])
    (match k
      [(scoord s end)
       (cons #`([#,s #,end]
                #,@(if capture
                       (filter (syntax-parser [(id _ ...) (memf (free-identifier=? #'id _) capture)]) v)
                       v))
             acc)])))

(define-for-syntax (unrender comp)
  (syntax-parse comp
    [({~datum comp} clauses ...)
     (for/list ([clause (syntax->list #'(clauses ...))])
       (syntax-parse clause
         [([start end] exprs ...)
          (cons (scoord (syntax->datum #'start) (syntax->datum #'end)) (syntax->list #'(exprs ...)))]))]
    [_ (error 'asdf "test")]))

(define FPS 44100)
(define ->frames (compose round (* FPS _)))

(define/match* (perform1 (coord (app ->frames start) (app ->frames end)) expr env sound)
  (define len (- end start))
  (match expr
    [(tone freq)
     (rs-overlay ((if (> start 0) (curry rs-append (silence start)) identity)
                  (rs-mult
                   (make-tone freq .2 len)
                   ((adsr 2 1.0 2 1.0 (round (* 1/4 len))) len)))
                 sound)]
    [rest
     (rs-overlay ((if (> start 0) (curry rs-append (silence start)) identity)
                  (silence len))
                 sound)]
    [_ sound]))

(define (perform comp env)
  (for*/fold ([acc (silence 1)])
             ([(k v) (in-dict comp)] [e v])
    (perform1 k e env acc)))

(define-syntax define-music
  (syntax-parser
    [(_ name exprs ...)
     #'(define-syntax name (syntax->list #'(exprs ...)))]))

(define-for-syntax (assemble* comp)
  (match comp
    [(cons layer layers)
     (match-define (scoord start end) (current-coordinate))
     (define l (unrender (realize #`(comp ([#,start #,end] #,@layer)))))
     (parameterize ([current-env (merge (current-env) l)])
       (merge l (assemble* layers)))]
    [_ '()]))

(define-syntax assemble
  (syntax-parser
    [(_ [start end] clauses ...)
     (parameterize ([current-coordinate (scoord (syntax->datum #'start) (syntax->datum #'end))])
       (render (assemble* (syntax->list #'(clauses ...)))
               performers))]))

#;(play
 (perform
  (assemble [0 4]
            [(loop (in [0 1] (tone (* 1000 (random)))))]
            #;[(comp ([0 1] (tone (* 1000 (random)))))])
  '()))

(define-music snip
  [(in [0 .5] (tone (! 0)))
   (in [.5 1] (tone (! 1)))
   (in [1 1.5] (tone (! 2)))
   (in [1.5 2] (tone (! 3)))
   (in [2 2.5] (tone (! 4)))
   (in [2.5 3] (tone (! 2)))
   (in [3 3.5] (tone (! 3)))
   (in [3.5 4] (tone (! 4)))])

(define-values (B C D E F G A B+ C+ D+ E+ F+)
  (values 246.94 261.63 293.66 329.63 349.23 392 440 493.88 523.25 587.33 659.26 698.46))

(play (perform
       (assemble [0 32]
                 [(in [0 8] (seq C E G C+ E+))
                  (in [8 16] (seq C D A D+ F+))
                  (in [16 24] (seq B D G D+ F+))
                  (in [24 32] (seq C E G C+ E+))]
                 [(loop 4 (music snip))])
      '()))
