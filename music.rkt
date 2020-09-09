#lang racket

(require (for-syntax fancy-app match-plus
                     racket/list racket/set racket/dict racket/match racket/struct-info
                     (only-in racket/control abort)
                     syntax/parse syntax/stx)
         syntax/parse fancy-app match-plus rsound rsound/envelope rsound/piano-tones
         racket/generic)

(provide coord tone rest
         comp in ! loop time-scale do do* -- seq restrict
         tone rest note midi
         define-music
         music
         perform1 perform
         FPS ->frames
         define-realizer define-performer
         coord
         (for-syntax translate score-length score-coord
                     merge1 merge
                     realize render assemble*
                     current-env current-coordinate find-all
                     within? scoord scoord-start scoord-end))

(struct coord [start end] #:transparent)

(begin-for-syntax

  (struct realizer [trans] #:transparent)
  (struct literal [] #:transparent)
  (struct music/s [layers] #:transparent)
  (struct scoord [start end start-stx end-stx] #:transparent)

  (define current-coordinate (make-parameter (scoord 0 +inf.0 #f #f)))
  (define current-env (make-parameter '()))
  
  (define (tee v) (println v) v)

  (define/match* (within? (scoord s1 e1 _ _) (scoord s2 e2 _ _))
    (and (>= s1 s2) (<= e1 e2) (not (= s1 e2))))

  (define (score-coord comp)
    (for/fold ([m1n +inf.0] [min-stx #f] [m4x -inf.0] [max-stx #f]
               #:result (scoord m1n m4x min-stx max-stx))
              ([k (in-dict-keys comp)])
      (match k [(scoord s e ss es)
                (define-values (m1n* min-stx*)
                  (if (> m1n s) (values s ss) (values m1n min-stx)))
                (define-values (m4x* max-stx*)
                  (if (< m4x e) (values e es) (values m4x max-stx)))
                (values m1n* min-stx* m4x* max-stx*)])))
  
  (define (score-length comp)
    (define-values [start end] (score-coord comp))
    (- end start))
  
  (define (translate comp t)
    (for/fold ([acc '()])
              ([(k v) (in-dict comp)])
      (match k
        [(scoord start end ss es)
         (foldr (merge1 (scoord (+ start t) (+ end t) ss es) _ _) acc v)])))
  
  (define (merge1 k v acc) (dict-update acc k (cons v _) '()))
  
  (define (merge comp1 comp2)
    (for*/fold ([acc comp2])
               ([(k v) (in-dict comp1)] [e v])
      (merge1 k e acc)))

  (define (find-all c0ord parser env)
    (for*/fold ([acc '()])
               ([(k v) (in-dict env)] #:when (within? c0ord k) [e v])
      (define result (parser e))
      (if result (cons result acc) acc)))
  
  ;; realize a score with the defined realizers.
  (define (realize expr)
    (syntax-parse expr
      [({~datum comp} clause* ...)
       (render
        (for/fold ([acc '()])
                  ([clause (syntax->list #'(clause* ...))])
          ;; TODO make a syntax class
          (syntax-parse clause
            [([startp endp] exprs ...)
             (match-define (list start end) (map syntax->datum (list #'startp #'endp)))
             (define (make-clauses expr)
               (syntax-parse expr
                 [({~datum comp} clauses ...)
                  (for/list ([clause (syntax->list #'(clauses ...))])
                    (syntax-parse clause
                      [([start*p end*p] exprs ...)
                       (match-define (list start* end*) (map syntax->datum (list #'start*p #'end*p)))
                       (when (> end* end)
                         (raise-syntax-error
                          'realize
                          (format "inner coordinate ends after outer. Max: ~s, Received: ~s" end end*)
                          #'end*p))
                       (cons (scoord start* end* #'start*p #'end*p) (syntax->list #'(exprs ...)))]))]
                 [expr (list (cons (scoord start end #'startp #'endp)
                                   (list #'expr)))]))
             (parameterize ([current-coordinate (scoord start end #'startp #'endp)])
               (foldr merge acc (stx-map (compose make-clauses realize) #'(exprs ...))))])))]
      [(id:id exprs ...)
       (define trans (syntax-local-value #'id (λ () #f)))
       (match trans
         [(literal) expr]
         [(realizer t) (realize (t expr))]
         [_ #`(#,@(stx-map realize #'(id exprs ...)))])]
      [(exprs ...) #`(#,@(stx-map realize #'(exprs ...)))]
      [expr*:id
       (match (syntax-local-value #'expr* (λ () #f))
         [(music/s m) (realize m)]
         [_ expr])]
      [_ expr]))

  ;; convert a score value to a syntactic score. If `for-performance` is true, all
  ;; non-performable toplevels are removed
  (define (render comp #:for-performance [for-performance #f])
    (for/fold ([acc '()] #:result #`(comp #,@acc))
              ([(k v) (in-dict comp)])
      (match k
        [(scoord s e ss es)
         (with-syntax ([s* (datum->syntax ss s ss)]
                       [e* (datum->syntax es e es)])
           (cons #`([s* e*]
                    #,@(if for-performance
                           (filter (syntax-parser
                                     [(id:id _ ...)
                                      (define info (syntax-local-value #'id (λ () #f)))
                                      (and (struct-info? info)
                                           (free-identifier=? #'performer (sixth (extract-struct-info info))))])
                                   v)
                           v))
                 acc))])))
  
  ;; convert a syntactic score to a score value.
  (define (unrender comp)
    (syntax-parse comp
      [({~datum comp} clauses ...)
       (for/list ([clause (syntax->list #'(clauses ...))])
         (syntax-parse clause
           [([start end] exprs ...)
            (cons (scoord (syntax->datum #'start) (syntax->datum #'end) #'start #'end) (syntax->list #'(exprs ...)))]))]
      [_ (error 'unrender "can only unrender a comp")])))

(define-generics performable (gen-perform performable start end))
(struct performer [] #:transparent)

(define-syntax define-performer
  (syntax-parser
    [(_ (id:id fields ... {~and {~datum start} start} {~and {~datum end} end}) body ...)
     #'(struct id performer [fields ...] #:transparent
         #:methods gen:performable
         [(define/match* (gen-perform (id fields ...) start end) body ...)])]))

(define-syntax define-realizer
  (syntax-parser
    [(_ id:id parser)
     #'(define-syntax id (realizer parser))]))

(define-syntax define-literal
  (syntax-parser
    [(_ id:id) #'(define-syntax id (literal))]))

;; make coordinates be relative to parent coordinates
(define-realizer in
  (syntax-parser
    [(_ [off-startp:number off-endp:number] exprs ...)
     (match-define (scoord start end _ _) (current-coordinate))
     (with-syntax ([start* (datum->syntax #'off-startp (+ start (syntax->datum #'off-startp)) #'off-startp)]
                   [end* (datum->syntax #'off-endp (+ start (syntax->datum #'off-endp)) #'off-endp)])
       #`(comp ([start* end*] exprs ...)))]))

;; loop a fragment as many times as possible in the surrounding space.
(define-realizer loop
  (syntax-parser
    [(_ {~optional length*:number #:defaults ([length* #'#f])} exprs ...)
     (match-define (scoord start end ss es) (current-coordinate))
     (define length (syntax->datum #'length*))
     (render
      ;; TODO improve
      (for/fold ([acc '()]
                 [prev (translate (list (cons (scoord start end ss es)
                                              (syntax->list #'(exprs ...))))
                                  (- length))]
                 #:result acc)
                ([i (in-range (floor (/ (- end start) length)))])
        (define new (translate prev length))
        (values (merge new acc) new)))]))

(define-literal seq)

;; try to get a value from a seq in the current environment.
(define-realizer !
  (syntax-parser
    [(_ ix:number)
     (define found
       (find-all (current-coordinate)
                 (syntax-parser
                   [({~datum seq} exprs ...) (syntax->list #'(exprs ...))]
                   [_ #f])
                 (current-env)))
     (if (null? found)
         (raise-syntax-error '! "no sequences in scope" this-syntax)
         (list-ref (car found) (syntax->datum #'ix)))]))

(define-realizer --
  (syntax-parser
    [(_ {~optional start*:number #:defaults ([start* #'0])} clauses* ...)
     (define start (syntax->datum #'start*))
     (define-values (result end)
       (for/fold ([clauses '()] [current 0])
                 ([clause (syntax->list #'(clauses* ...))])
         (syntax-parse clause
           [(l*:number {~optional {~or* {~or {~seq #:elide elide*:number}
                                             {~seq #:overlap overlap*:number}}
                                        {~seq #:elide-left elidel*:number}}} exprs ...)
            (define next (+ current (syntax->datum #'l*)))
            (let-values ([(start start-stx)
                           (if (attribute elidel*)
                               (values (+ current (syntax-e #'elidel*)) #'elidel*)
                               (values current #'l*))]
                          [(end end-stx)
                           (if (attribute elide*)
                               (values (- next (syntax-e #'elide*)) #'elide*)
                               (values next #'l*))])
              (with-syntax ([current* (datum->syntax #'l* current #'l*)]
                            [next* (datum->syntax #'l* next #'l*)]
                            [start* (datum->syntax start-stx start start-stx)]
                            [end* (datum->syntax end-stx end end-stx)])
                (values (cons #`(do* [(in [current* next*] exprs ...)]
                                     [(restrict start* end*)])
                              clauses)
                        (+ next (- current start) (- end next)
                           (if (attribute overlap*) (- (syntax->datum #'overlap*)) 0)))))])))
     (with-syntax ([end* (datum->syntax this-syntax (+ end start) this-syntax)]
                   [(result* ...) (datum->syntax this-syntax result this-syntax)])
       #`(in [start* end*] result* ...))]))

(define-realizer time-scale
  (syntax-parser
    [(_ time*:number)
     (define time (syntax->datum #'time*))
     (match-define (and coord (scoord s* _ _ _)) (current-coordinate))
     (render
      (for/list ([(k v) (in-dict (current-env))] #:when (within? k coord))
        (match k
          [(scoord s e ss es)
           (define new-s (+ s (* (sub1 time) (- s s*))))
           (cons (scoord new-s (+ new-s (* time (- e s))) ss es) v)])))]))

(define-realizer restrict
  (syntax-parser
    [(_ start:number end:number)
     (match-define (scoord start* _ _ _) (current-coordinate))
     (render (filter (compose (within? _ (scoord (+ (syntax->datum #'start) start*)
                                                 (+ (syntax->datum #'end) start*) #f #f))
                              car)
                     (current-env)))]))

;; embed a piece of music defined with `define-music` into a score.
(define-realizer do
  (syntax-parser
    [(_ [exprs ...] ...) (render (assemble* (syntax->list #'((exprs ...) ...))))]))

;; embed a piece of music defined with `define-music` into a score, keeping only
;; the last layer
(define-realizer do*
  (syntax-parser
    [(_ [exprs ...] ...) (render (assemble*/keep-last (syntax->list #'((exprs ...) ...))))]))

(define-realizer note
  (syntax-parser
    [(_ pitch:id octave:number) #'(note pitch 0 octave)]
    [(_ p:id accidental:number o:number)
     #`(midi #,(+ (match (syntax->datum #'p) ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11])
                  (syntax->datum #'accidental)
                  (* 12 (syntax->datum #'o))
                  12))]))

(define-performer (midi semitone start end)
  (define len (- end start))
  (values start
          (rs-mult
           (piano-tone semitone)
           ((adsr 2 1.0 2 1.0 (round (* 1/4 len))) len))))

;; expand a syntactic score to a usable value
(define-syntax comp
  (syntax-parser
    [(_ ([start end] exprs ...) ...)
     #'(list (cons (coord start end) (list exprs ...)) ...)]))

(define FPS 44100)
(define ->frames (compose round (* FPS _)))

(define-performer (tone freq start end)
  (define len (- end start))
  (values start
          (rs-mult
           (make-tone freq .2 len)
           ((adsr 2 1.0 2 1.0 (round (* 1/4 len))) len))))

(define-performer (rest start end) (values start (silence (- end start))))

;; perform one expression using the defined performers.
(define/match* (perform1 (coord (app ->frames start) (app ->frames end)) expr sound)
  (define len (- end start))
  (cond [(performable? expr)
         (define-values (start* sound*) (gen-perform expr start end))
         (rs-overlay ((if (> start* 0) (curry rs-append (silence start*)) identity) sound*)
                     sound)]
        [else (error 'perform "cannot perform ~s" expr)]))

;; perform a score.
(define (perform comp)
  (for*/fold ([acc (silence 1)])
             ([(k v) (in-dict comp)] [e v])
    (perform1 k e acc)))

;; define a syntax value containing layers, which can later be assembled into music.
(define-syntax define-music
  (syntax-parser [(_ name expr) #`(define-syntax name #,(music/s #'expr))]))

;; realize a composition by layer, using the cumulative result as the environment
;; for the next layer.  Results in a score value.
(define-for-syntax (assemble* comp)
  (match comp
    [(cons layer layers)
     (match-define (scoord start end ss es) (current-coordinate))
     (define l
       (with-syntax ([s* (datum->syntax ss start ss)]
                     [e* (datum->syntax es end es)])
         (unrender (realize #`(comp ([s* e*] #,@layer))))))
     (parameterize ([current-env (merge (current-env) l)]) (merge l (assemble* layers)))]
    [_ '()]))

(define-for-syntax (assemble*/keep-last comp)
  (match comp
    [(list* layer next more)
     (match-define (scoord start end ss es) (current-coordinate))
     (define l
       (unrender
        (realize
         (with-syntax ([s* (datum->syntax ss start ss)]
                       [e* (datum->syntax es end es)])
           #`(comp ([s* e*] #,@layer))))))
     (parameterize ([current-env (merge (current-env) l)])
       (assemble*/keep-last (cons next more)))]
    [(cons layer '())
     (match-define (scoord start end ss es) (current-coordinate))
     (unrender
      (realize (with-syntax ([s* (datum->syntax ss start ss)]
                             [e* (datum->syntax es end es)])
                 #`(comp ([s* e*] #,@layer)))))]
    [_ '()]))

;; assemble a score within the given coordinate, resulting in a syntactic score.
(define-syntax music (syntax-parser [(_ expr) (render (unrender (realize #'expr)) #:for-performance #t)]))
