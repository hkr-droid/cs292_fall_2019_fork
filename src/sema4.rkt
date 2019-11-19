#lang rosette

(require rosette/lib/angelic)
(require rosette/lib/match)

;; env is a map of maps
; given a tag, look up the map for that tag
; (e.g. 'thread1, 'global, 'semaphore)
; then return the value for the given key
(define (lookup-env env tag key)
  (hash-ref (hash-ref env tag) key))

; given an environment, tag, key, value, 
; look up the map for the given tag, 
; and set val for the given key
(define (set-env env tag key val)
  (hash-set env tag 
			(hash-set (hash-ref env tag) key val)))

; syntax for arithmetic expressions
(struct add (left right) #:transparent)
(struct sub (left right) #:transparent)
(struct mul (left right) #:transparent)

; evaluate arithmetic expressions (result is number)
(define (aeval exp env tag)
  (match exp
    [(? symbol?)    (lookup-env env tag exp)]
    [(? number?)     exp]
    [(add a b)      (+ (aeval a env tag) (aeval b env tag))]
    [(sub a b)      (- (aeval a env tag) (aeval b env tag))]
    [(mul a b)      (* (aeval a env tag) (aeval b env tag))]
))

; syntax for boolean expressions
(struct beq  (a b) #:transparent)
(struct ble  (a b) #:transparent)
(struct bnot (a)   #:transparent)
(struct band (a b) #:transparent)

; evaluate boolean expressions (result is boolean)
(define (beval exp env tag)
  (match exp
    [(? boolean?)    exp]
    [(beq a b)       (= (aeval a env tag) (aeval b env tag))]
    [(ble a b)       (<= (aeval a env tag) (aeval b env tag))]
    [(bnot a)        (not (beval a env tag))]
    [(band a b)      (and (beval a env tag) (beval b env tag))]
))

; syntax for commands
(struct skip   ()          #:transparent) ; skip (no-op)
(struct :=    (left right) #:transparent) ; assignment
(struct :     (c1 c2)      #:transparent) ; sequence commands
(struct ite   (e c1 c2)    #:transparent) ; if then else
(struct loop  (e c)        #:transparent) ; loop command c on expression e
(struct get   (g v)        #:transparent) ; load a global variable into a local variable value
(struct store (v g)        #:transparent) ; store a global variable with a local variable value

; interpret commands (returns resulting environment)
(define (interpret com env tag)
  (match com
    [(skip)            env]
    [(:= v e)          (set-env env tag v (aeval e env tag))]
    [(: c1 c2)         (interpret c2 (interpret c1 env tag) tag)]
    [(ite e c1 c2)     (if (beval e env tag) (interpret c1 env tag) (interpret c2 env tag))]
    [(loop e c)        (let looper ([loop-env env])
                            (if (beval e loop-env tag)
                                (looper (interpret c loop-env tag))
                                (interpret (skip) loop-env tag)))]
    [(get g v)         (set-env env tag v (lookup-env env 'global g))]
    [(store v g)       (set-env env 'global g (lookup-env env tag v))]
))

; empty environment
(define env-empty  (hash))
(define t1-env
  (hash-set (hash) 't1 (hash)))

; example program
(define com1 (: (:= 'a 5) (:= 'b (sub (add 7 4) 'a))))

; runs the following program:
;   a := 5
;   b := (7 + 4) - a
; Result should be a env with { a -> 5 and b -> 6 }
(printf "Test 1:~n a := 5 ; b := (7 + 4) - a ~n")
(interpret com1 (hash-set (hash) 't1 (hash)) 't1)
; bunch of skips makes no difference
(printf " skip ; skip ; a := 5 ; b := (7 + 4) - a ~n")
(interpret (: (skip) (: (skip) com1)) t1-env 't1)
(printf "~n")

; syntax is ugly as sin, but if you write it like so
; it becomes a little bit more tolerable, and it looks so happy! (:
(define com2 (: (:= 'a 5)
             (: (:= 'b 50)
             (: (:= 'c (add 'a 'b))
                (skip)))))
(printf "Test 2:~n a := 5 ; b := 50 ; c := a + b ; skip ~n")
(interpret com2 t1-env 't1)
(printf "~n")

(define com3 (: (:= 'a 5)
             (: (:= 'b 50)
             (: (:= 'a (add 'a 1))
             (: (:= 'b (add 'b 1))
             (: (:= 'b (add 'b 1))
                (skip)))))))
(printf "Test 3:~n a := 5 ; b := 50 ; a := a + 1; b := b + 1 ; b := b + 1 ; skip ~n")
(interpret com3 t1-env 't1)
(printf "~n")

; if-then-else test
(define com4true (: (:= 'a 5)
                 (: (:= 'b 50)
                    (ite (and (ble 'a 'b) (beq (mul 'a 10) 'b))
                         (:= 'a (add 'a 1))
                         (:= 'b (add 'b 1))))))
(printf "Test 4 true branch:~n a := 5 ; b := 50 ; if a <= b and (a * 10 = b) then a := a + 1 else b := b + 1 ~n")
(interpret com4true t1-env 't1)
(define com4false (: (:= 'a 5)
                  (: (:= 'b 50)
                     (ite (bnot (ble 'a 'b))
                          (:= 'a (add 'a 1))
                          (:= 'b (add 'b 1))))))
(printf "Test 4 false branch:~n a := 5 ; b := 50 ; if !(a <= b) then a := a + 1 else b := b + 1 ~n")
(interpret com4false t1-env 't1)
(printf "~n")

(define com5 (: (:= 'a 5)
             (: (:= 'b 50)
             (: (loop (ble 'a 'b)
                      (:= 'a (add 'a 1)))
                (:= 'a (sub 'a 1))))))
(printf "Test 5 loop:~n a := 5 ; b := 50 ; while a <= b do a := (a + 1) end ; a := a - 1 ~n")
(interpret com5 t1-env 't1)
(printf "~n")

; testing TWO sequenced commands in a loop body
(define com6 (: (:= 'a 5)
             (: (:= 'b 50)
             (: (loop (ble 'a 'b)
                      (: (:= 'a (add 'a 1))
						 (:= 'b (sub 'b 1))))
                (skip)))))
(printf "Test 6 loop:~n a := 5 ; b := 50 ; while a <= b do a := (a + 1) ; b := (b - 1) end ; skip ~n")
(interpret com6 t1-env 't1)
(printf "~n")

(define global-env
  (hash-set (hash-set (hash) 'A 5) 'B 50))
(define test-env
  (hash-set (hash-set (hash) 't1 (hash)) 'global global-env))

(define com7 (: (get 'A 'a)
             (: (:= 'a (add 'a 1))
                (store 'a 'A))))
(printf "Test 7 loop:~n get A a ; a := a + 1 ; store a A ~n")
(interpret com7 test-env 't1)
(printf "Test 7b loop:~n(thread 1) get A a ; a := a + 1 ; store a A ~n(thread 2) get A a ; a := a + 1 ; store a A ~n")
(interpret com7 (interpret com7 (hash-set test-env 't2 (hash)) 't1) 't2)
(printf "~n")

; to do: add rosette symbolics
;(define-symbolic c x y integer?)
;(aeval (mul x (add y 2)))
