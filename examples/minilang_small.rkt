#lang racket

(require racket/match racket/struct)

(struct add (left right) #:transparent)
(struct 止 () #:transparent)

(struct state (c e k)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'state)
      (lambda (obj) (list (state-c obj) (state-e obj) (state-k obj)))))])

(define (step s)
  (match s
    
    [(state (cons (? number? n) c) e k)         (state c e (cons n k))]

    ;[((list-ref ?boolean? c) e k)                     (state c e (cons ?boolean? k))]
    ;[(?symbol? e k)	                              (state (env-lookup ?symbol? e) e k)]  ;; todo ensure lookup works
    ;[(`(lambda ,vs ,e) e k)	                      (state (closure lambda ,vs ,e) e k)]

    ;; arithmetic operations
    [(state (cons `(add ,E1 ,E2) c) e k)            (state (cons E1 (cons E2 (cons add c))) e k)]
    [(state (cons add c) e (cons n2 (cons n1 k)))   (state c e (cons (+ n1 n2) k))]
    ;[`((cons ('+ ,E1 ,E2) ,c) ,e ,k)                   (state (cons E1 (cons E2 (cons '+ c))) e k)]
    ;[`((cons '+ ,c) ,e (cons ,n2 ,n1 ,k))          (state c e (cons (+ n1 n2) k))]
    
    ;; todo boolean operations
    
    ;[((list-ref skip c) e k)                          (state c e k)
    ;[((list-ref (ite b c0 c1) c) e k)                 (state (cons b (cons ite c)) e (cons c0 (cons c1 k)))]
    ;[((list-ref ite c) e (list-ref #t c0 c1 k) )    (state c0 e k)]
    ;[((list-ref ite c) e (list-ref #f c0 c1 k) )   (state c1 e k)]
    ;[((list-ref follows c0 c1 c) e k)	              (state (cons c0 (cons c1 c)) e k)]
    ;; todo [(assign ,v ,e)	          (env-set! env v e)]
    ;; todo while

    [(state (cons 止 _) e k)                  (state '() e k)]  ;; not sure this is matching?
))

(define p1 (list 1 止))
(step (step (state p1 (list) (list))))

(define p2 '( (add 1 2) ))
(define s (state p2 (list) (list)))
(step s)
(step (step s))
(step (step (step s)))
(step (step (step (step s))))