#lang racket

					; This is the lambda calculus interpreter written by Matt Might

					; bring in the match library:
(require racket/match)

					; eval matches on the type of expression:
(define (eval exp env)
  (match exp
	 [`(,f ,e)
	  (apply (eval f env)
		 (eval e env))]
	 [`(λ ,v . ,e)
	  `(closure ,exp ,env)]
					;[`(add e1 e2)    (

	 [(?symbol?)
	  (cadr (assq exp env))]
					;[(? number?)      exp]
	 ))

					; apply destructures the function with a match too:
(define (apply f x)
  (match f
	 [`(closure (λ ,v . ,body)
		    ,env)
	  (eval body
		(cons `(,v ,x) env))]))

					; read in, parse and evaluate:
(display (eval (read)
	       '()))    (newline)

					;(λ x . x)
					;(((λ f . (λ x . (f x))) (λ a . a)) (λ b . b))
					;(λ x . (+ x 1))
