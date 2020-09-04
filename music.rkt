#lang racket

(require (for-syntax fancy-app match-plus
                     racket/list racket/set racket/dict racket/match
                     syntax/parse syntax/stx syntax/id-table)
         syntax/parse fancy-app match-plus rsound rsound/envelope
         racket/generic)

(provide coord tone rest
         comp in ! offset loop music
         tone rest
         translate score-length
         merge1 merge
         define-music
         assemble-score
         (for-syntax realize render assemble*)
         perform1 perform
         FPS ->frames)

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
  (define current-env (make-parameter '()))
  (struct realizer [trans] #:transparent))

(define-generics performable (gen-perform performable start end))

(define-syntax define-performer
  (syntax-parser
    [(_ (id:id fields ... {~and {~datum start} start} {~and {~datum end} end}) body ...)
     #'(struct id [fields ...] #:transparent
         #:methods gen:performable
         [(define/match* (gen-perform (id fields ...) start end) body ...)])]))

(define-syntax define-realizer
  (syntax-parser
    [(_ id:id parser)
     #'(define-syntax id (realizer parser))]))

;; make coordinates be relative to parent coordinates
(define-realizer in
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

;; loop a fragment as many times as possible in the surrounding space.
(define-realizer loop
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

;; translate a score
(define-realizer offset
  (syntax-parser
    [(_ n)
     (for/fold ([acc '()])
               ([(k v) (in-dict (current-env))] #:when (within? k (current-coordinate)))
       (define tones (filter (compose (eq? _'tone) car syntax->datum) v))
       (cons (cons k (map (syntax-parser [({~datum tone} expr*) #'(tone (+ expr* n))])
                          tones))
             acc))]))

;; try to get a value from a sequence in the current environment.
(define-realizer !
  (syntax-parser
    [(_ ix)
     (car (find-all (current-coordinate)
                    (syntax-parser
                      [({~datum seq} exprs ...)
                       (list-ref (syntax->list #'(exprs ...))
                                 (syntax->datum #'ix))]
                      [_ #f])
                    (current-env)))]))

;; embed a piece of music defined with `define-music` into a score.
(define-realizer music
  (syntax-parser
    [(_ id:id) (render (assemble* (syntax-local-value #'id)))]))

#;(define-realizer note
  (syntax-parser
    [(_ pitch accidental octave)]))

;; realize a score with the defined realizers.
(define-for-syntax (realize expr)
  (define expr*
    (syntax-parse expr
      [(id:id _ ...)
       (define trans (syntax-local-value #'id (λ () #f)))
       (println trans)
       (match trans
         [(realizer t) (realize (t expr))]
         [_ expr])]
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

;; expand a syntactic score to a usable value
(define-syntax comp
  (syntax-parser
    [(_ ([start end] exprs ...) ...)
     #'(list (cons (coord start end) (list exprs ...)) ...)]))

;; convert a score value to a syntactic score.
(define-for-syntax (render comp #:for-performance [for-performance #f])
  (for/fold ([acc '()] #:result #`(comp #,@acc))
            ([(k v) (in-dict comp)])
    (match k
      [(scoord s end)
       (cons #`([#,s #,end]
                #,@(if for-performance
                       ;; good enough for now
                       (filter (syntax-parser [(id _ ...) (syntax-local-value #'id (λ () #f))]) v)
                       v))
             acc)])))

;; convert a syntactic score to a score value.
(define-for-syntax (unrender comp)
  (syntax-parse comp
    [({~datum comp} clauses ...)
     (for/list ([clause (syntax->list #'(clauses ...))])
       (syntax-parse clause
         [([start end] exprs ...)
          (cons (scoord (syntax->datum #'start) (syntax->datum #'end)) (syntax->list #'(exprs ...)))]))]
    [_ (error 'unrender "can only unrender a comp")]))

(define FPS 44100)
(define ->frames (compose round (* FPS _)))

(define-performer (tone freq start end)
  (define len (- end start))
  ((if (> start 0) (curry rs-append (silence start)) identity)
   (rs-mult
    (make-tone freq .2 len)
    ((adsr 2 1.0 2 1.0 (round (* 1/4 len))) len))))

(define-performer (rest start end)
  ((if (> start 0) (curry rs-append (silence start)) identity)
   (silence (- end start))))

;; perform one expression using the defined performers.
(define/match* (perform1 (coord (app ->frames start) (app ->frames end)) expr env sound)
  (define len (- end start))
  (if (performable? expr) (rs-overlay (gen-perform expr start end) sound) sound))


;; perform a score in the given environment.
(define (perform comp env)
  (for*/fold ([acc (silence 1)])
             ([(k v) (in-dict comp)] [e v])
    (perform1 k e env acc)))

;; define a syntax value containing layers, which can later be assembled into music.
(define-syntax define-music
  (syntax-parser
    [(_ name exprs ...)
     #'(define-syntax name (syntax->list #'(exprs ...)))]))

;; realize a composition by layer, using the cumulative result as the environment
;; for the next layer.  Results in a score value.
(define-for-syntax (assemble* comp)
  (match comp
    [(cons layer layers)
     (match-define (scoord start end) (current-coordinate))
     (define l (unrender (realize #`(comp ([#,start #,end] #,@layer)))))
     (parameterize ([current-env (merge (current-env) l)])
       (merge l (assemble* layers)))]
    [_ '()]))

;; assemble a score within the given coordinate, resulting in a syntactic score.
(define-syntax assemble-score
  (syntax-parser
    [(_ [start end] clauses ...)
     (parameterize ([current-coordinate (scoord (syntax->datum #'start) (syntax->datum #'end))])
       (render (assemble* (syntax->list #'(clauses ...)))
               #:for-performance #t))]))
