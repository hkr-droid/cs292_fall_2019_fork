#lang rosette

(require rosette/lib/angelic)
(require rosette/lib/match)

					; syntax for expressions
(struct add
	(left right)
	#:transparent)
(struct sub
	(left right)
	#:transparent)
(struct mul
	(left right)
	#:transparent)

					; evaluate expressions (result is number)
(define (eval exp env)
  (match exp
	 [(?symbol?)
	  (hash-ref env exp)]
	 [(?number?) exp]
	 [(add a b)
	  (+ (eval a env)
	     (eval b env))]
	 [(sub a b)
	  (- (eval a env)
	     (eval b env))]
	 [(mul a b)
	  (* (eval a env)
	     (eval b env))]))

					; empty environment
(define (env-empty)
  (hash))

					; example
(define exp1
  (sub (add 7 4)
       'a))
(define env
  (hash))

					; evaluate exp1: (7 + 4) - a = 6
(eval exp1
      (hash-set env 'a 5))

					; syntax for commands
(struct skip
	()
	#:transparent)
(struct := (left right)#:transparent)
(struct seq
	(c1 c2)
	#:transparent)

					; interpret commands (returns resulting environment)
(define (interpret com env)
  (match com
	 [(skip) env]
	 [(:= v e)
	  (hash-set env
		    v
		    (eval e env))]
	 [(seq c1 c2)
	  (interpret c2
		     (interpret c1 env))]))

					; example program
(define com1
  (seq (:= 'a 5)
       (:= 'b exp1)))

					; runs the following program:
					;   a := 5
					;   b := (7 + 4) - a
					; Result should be a state with { a -> 5 and b -> 6 }
(interpret com1
	   (env-empty))

					;(interpret (seq (skip) (seq (skip) com1)) (env-empty))

					; to do: add rosette symbolics
					;(define-symbolic c x y integer?)
					;(eval (mul x (add y 2)))
