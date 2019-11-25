#lang racket

(require racket/match)

; This is the Scheme-subset interpreter written by Matt Might
;; Evaluation toggles between eval and apply.


;; todo need these 
(struct W
  | const n
  | closure
  )
(struct state   (c e k))
(struct closure (f e))
(struct EvalArg (m e))  ;; currently evaluating e1, next to evaluate e2
(struct Call (m e))     ;; evaluated e1 to \x.t and are working on e2, next call

(define (step s)
  (match s
    [((list-ref ?number? c) e k)                      (state c e (cons ?number? k))]
    [((list-ref ?boolean? c) e k)                     (state c e (cons ?boolean? k))]
    [(?symbol? e k)	                              (state (env-lookup ?symbol? e) e k)]  ;; todo ensure lookup works
    [(`(lambda ,vs ,e) e k)	                      (state (closure lambda ,vs ,e) e k)]  ;; todo whats w/ the commas
    [((app m0 m1) e k)                                (state m0 e (cons (EvalArg m1 e) k))]

    ;; todo how to treat W?
    [(W e (list-ref (EvalArg m e) k))                 (state m e (cons (Call W) k))]
    [(W e (cons (closure lambda ,m ,e2) k))           (state m (env-extend e2 W) k)]

    ;; todo arithmetic operations
    [((list-ref (add n1 n2) c) e k)                   (state (cons n1 (cons n2 (cons add c))) e k)]
    x[((list-ref add c) e (list-ref n1 n2 k))          (state c e (cons (+ n1 n2)))]
    
    ;; todo boolean operations
    
    [((list-ref skip c) e k)                          (state c e k)
    [((list-ref (ite b c0 c1) c) e k)                 (state (cons b (cons ite c)) e (cons c0 (cons c1 k)))]
    [((list-ref ite c) e (list-ref #t c0 c1 k) )    (state c0 e k)]
    [((list-ref ite c) e (list-ref #f c0 c1 k) )   (state c1 e k)]
    [((list-ref follows c0 c1 c) e k)	              (state (cons c0 (cons c1 c)) e k)]
    ;; todo [(assign ,v ,e)	          (env-set! env v e)]
    ;; todo while
    ))


; eval dispatches on the type of expression:
(define (eval exp env)
  (match exp
    [(?symbol? e k)	          (env-lookup env exp)]
    [(?number?)              exp]
    [(?boolean?)             exp]
    [`(if ,ec ,et ,ef)       (if (eval ec env) (eval et env) (eval ef env))]
    [`(letrec ,binds ,eb)	  (eval-letrec binds eb env)]
    [`(let ,binds ,eb)	  (eval-let binds eb env)]
    [`(lambda ,vs ,e)	  `(closure ,exp ,env)]
    [`(set! ,v ,e)	          (env-set! env v e)]
    [`(begin ,e1 ,e2)	  (begin (eval e1 env) (eval e2 env))]
    [`(,f . ,args)	          (apply-proc (eval f env) (map (eval-with env) args))]
    ))

; a handy wrapper for Currying eval:
(define (eval-with env)
  (lambda (exp)
    (eval exp env)))

; eval for letrec:
(define (eval-letrec bindings body env)
  (let* ((vars (map car bindings))
	 (exps (map cadr bindings))
	 (fs (map (lambda _ #f)
		  bindings))
	 (env* (env-extend* env vars fs))
	 (vals (map (eval-with env*)
		    exps)))
    (env-set!* env* vars vals)
    (eval body env*)))

; eval for let:
(define (eval-let bindings body env)
  (let* ((vars (map car bindings))
	 (exps (map cadr bindings))
	 (vals (map (eval-with env)
		    exps))
	 (env* (env-extend* env vars vals)))
    (eval body env*)))

; applies a procedure to arguments:
(define (apply-proc f values)
  (match f
	 [`(closure (lambda ,vs ,body)
		    ,env)
          ; =>
	  (eval body
		(env-extend* env vs values))]
	 [`(primitive ,p)
          ; =>
	  (apply p values)]))

;; Environments map variables to mutable cells 
;; containing values.

(define-struct cell
  ([value #:mutable]))

; empty environment:
(define (env-empty)
  (hash))

; initial environment, with bindings for primitives:
(define (env-initial)
  (env-extend* (env-empty)
	       '(+ - / * <= void display newline)
	       (map (lambda (s)
		      (list 'primitive s))
		    `(,+ ,- ,/ ,* ,<= ,void ,display ,newline))))

; looks up a value:
(define (env-lookup env var)
  (cell-value (hash-ref env var)))

; sets a value in an environment:
(define (env-set! env var value)
  (set-cell-value! (hash-ref env var)
		   value))

; extends an environment with several bindings:
(define (env-extend* env vars values)
  (match `(,vars ,values)
	 [`((,v . ,vars)
	    (,val . ,values))
          ; =>
	  (env-extend* (hash-set env
				 v
				 (make-cell val))
		       vars
		       values)]
	 [`(()
	    ())
          ; =>
	  env]))

; mutates an environment with several assignments:
(define (env-set!* env vars values)
  (match `(,vars ,values)
	 [`((,v . ,vars)
	    (,val . ,values))
          ; =>
	  (begin (env-set! env v val)
		 (env-set!* env vars values))]
	 [`(()
	    ())
          ; =>
	  (void)]))

;; Evaluation tests.
; define new syntax to make tests look prettier:
(define-syntax test-eval
  (syntax-rules (====)
		[(_ program ==== value)
		 (let ((result (eval (quote program)
				     (env-initial))))
		   (when (not (equal? program value))
		     (error "test failed!")))]))

(test-eval ((lambda (x)
	      (+ 3 4)) 20)
	   ====
	   7)

(test-eval (letrec ((f (lambda (n)
			 (if (<= n 1)
			     1
			   (* n
			      (f (- n 1)))))))
	     (f 5))
	   ====
	   120)

(test-eval (let ((x 100))
	     (begin (set! x 20)
		    x))
	   ====
	   20)

(test-eval (let ((x 1000))
	     (begin (let ((x 10))
		      20)
		    x))
	   ====
	   1000)

;; Programs are translated into a single letrec expression.
(define (define->binding define)
  (match define
	 [`(define (,f . ,formals)
	     ,body)
          ; =>

	  `(,f
	    (lambda ,formals ,body))]
	 [`(define ,v ,value)
          ; =>

	  `(,v ,value)]
	 [else
          ; =>
	  `(,(gensym)
	    ,define)]))

(define (transform-top-level defines)
  `(letrec ,(map define->binding defines)
     (void)))

(define (eval-program program)
  (eval (transform-top-level program)
	(env-initial)))

(define (read-all)
  (let ((next (read)))
    (if (eof-object? next)
	'()
      (cons next (read-all)))))

; read in a program, and evaluate:
(eval-program (read-all))
;(eval-program `((lambda (x) (+ 3 4)) 20))
; (define a 5) (define b 50) (let ([x a] [y b]) (begin (set! a (+ x y)) a))
