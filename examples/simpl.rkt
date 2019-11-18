#lang rosette

(require rosette/lib/angelic)
(require rosette/lib/match)

; syntax for arithmetic expressions
(struct add (left right) #:transparent)
(struct sub (left right) #:transparent)
(struct mul (left right) #:transparent)

; evaluate arithmetic expressions (result is number)
(define (aeval exp env)
  (match exp
    [(? symbol?)    (hash-ref env exp)]
    [(? number?)     exp]
    [(add a b)      (+ (aeval a env) (aeval b env))]
    [(sub a b)      (- (aeval a env) (aeval b env))]
    [(mul a b)      (* (aeval a env) (aeval b env))]
))

; syntax for boolean expressions
(struct beq  (a b) #:transparent)
(struct ble  (a b) #:transparent)
(struct bnot (a)   #:transparent)
(struct band (a b) #:transparent)

; evaluate boolean expressions (result is boolean)
(define (beval exp env)
  (match exp
    [(? boolean?)    exp]
    [(beq a b)       (= (aeval a env) (aeval b env))]
    [(ble a b)       (<= (aeval a env) (aeval b env))]
    [(bnot a)        (not (beval a env))]
    [(band a b)      (and (beval a env) (beval b env))]
))

; syntax for commands
(struct skip ()          #:transparent) ; skip (no-op)
(struct :=  (left right) #:transparent) ; assignment
(struct :   (c1 c2)      #:transparent) ; sequence commands
(struct ite (e c1 c2)    #:transparent) ; if then else
(struct loop (e c)       #:transparent) ; loop command c on expression e
;(struct load (g v) #:transparent)      ; load a global variable into a local variable value
;(struct store (v g) #:transparent)     ; store a global variable with a local variable value

					; interpret commands (returns resulting environment)
(define (interpret com env)
  (match com
    [(skip)            env]
    [(:= v e)          (hash-set env v (aeval e env))]
    [(: c1 c2)         (interpret c2 (interpret c1 env))]
    [(ite e c1 c2)     (if (beval e env) (interpret c1 env) (interpret c2 env))]
    [(loop e c)        (let looper ([loop-env env])
                            (if (beval e loop-env)
                                (looper (interpret c loop-env))
                                (interpret (skip) loop-env)))]
    ;[(load g v)      env]
    ;[(store v g)     env]
))

; empty environment
(define (env-empty)  (hash))

; example program
(define com1 (: (:= 'a 5) (:= 'b (sub (add 7 4) 'a))))

; runs the following program:
;   a := 5
;   b := (7 + 4) - a
; Result should be a state with { a -> 5 and b -> 6 }
(printf "Test 1:~n a := 5 ; b := (7 + 4) - a ~n")
(interpret com1 (env-empty))
; bunch of skips makes no difference
(printf " skip ; skip ; a := 5 ; b := (7 + 4) - a ~n")
(interpret (: (skip) (: (skip) com1)) (env-empty))
(printf "~n")

; syntax is ugly as sin, but if you write it like so
; it becomes a little bit more tolerable, and it looks so happy! (:
(define com2 (: (:= 'a 5)
             (: (:= 'b 50)
             (: (:= 'c (add 'a 'b))
                (skip)))))
(printf "Test 2:~n a := 5 ; b := 50 ; c := a + b ; skip ~n")
(interpret com2 (env-empty))
(printf "~n")

(define com3 (: (:= 'a 5)
             (: (:= 'b 50)
             (: (:= 'a (add 'a 1))
             (: (:= 'b (add 'b 1))
             (: (:= 'b (add 'b 1))
                (skip)))))))
(printf "Test 3:~n a := 5 ; b := 50 ; a := a + 1; b := b + 1 ; b := b + 1 ; skip ~n")
(interpret com3 (env-empty))
(printf "~n")

; if-then-else test
(define com4true (: (:= 'a 5)
                 (: (:= 'b 50)
                    (ite (and (ble 'a 'b) (beq (mul 'a 10) 'b))
                         (:= 'a (add 'a 1))
                         (:= 'b (add 'b 1))))))
(printf "Test 4 true branch:~n a := 5 ; b := 50 ; if a <= b and (a * 10 = b) then a := a + 1 else b := b + 1 ~n")
(interpret com4true (env-empty))
(define com4false (: (:= 'a 5)
                  (: (:= 'b 50)
                     (ite (bnot (ble 'a 'b))
                          (:= 'a (add 'a 1))
                          (:= 'b (add 'b 1))))))
(printf "Test 4 false branch:~n a := 5 ; b := 50 ; if !(a <= b) then a := a + 1 else b := b + 1 ~n")
(interpret com4false (env-empty))
(printf "~n")

(define com5 (: (:= 'a 5)
             (: (:= 'b 50)
             (: (loop (ble 'a 'b)
                      (:= 'a (add 'a 1)))
                (:= 'a (sub 'a 1))))))
(printf "Test 5 loop:~n a := 5 ; b := 50 ; while a <= b do a := (a + 1) end ; a := a - 1 ~n")
(interpret com5 (env-empty))
(printf "~n")

; to do: add rosette symbolics
;(define-symbolic c x y integer?)
;(aeval (mul x (add y 2)))
