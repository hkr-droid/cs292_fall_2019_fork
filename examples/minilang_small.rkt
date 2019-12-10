#lang racket

(require racket/match racket/struct)

;; env is a map of maps
;; given a tag, look up the map for that store 
;; (e.g. 'thread1, 'global, 'semaphore)
;; then return the value for the given key
(define (lookup-env env store key)
  (hash-ref (hash-ref env store) key))

;; given an environment, store, key, value, 
;; look up the map for the given tag, 
;; and set val for the given key
(define (set-env env store key val)
  (hash-set env store
            (hash-set (hash-ref env store) key val)))

;; syntax
(struct add (e1 e2) #:transparent)
(struct sub (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)

(struct ¬ (b) #:transparent)
(struct ∧ (b1 b2) #:transparent)

(struct =? (e1 e2) #:transparent)
(struct ≤ (e1 e2) #:transparent)

(struct skip () #:transparent)
(struct := (left right) #:transparent)
(struct : (c1 c2) #:transparent)
(struct ite (b c1 c2) #:transparent)
(struct while (b c) #:transparent)
(struct 止 () #:transparent)  ;; as in 止まれ (tomare), halt

;; state struct with pretty printing
(struct state (c e k)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'state)
      (lambda (obj) (list (state-c obj) (state-e obj) (state-k obj)))))])

;; step function
(define (step s)
  (match s

    ;; values
    [(state (list-rest (? number? n) c) e k)            (state c e (cons n k))]
    [(state (list-rest (? boolean? b) c) e k)           (state c e (cons b k))]

    ;; arithmetic operations
    [(state (list-rest (add E1 E2) c) e k)              (state (cons E1 (cons E2 (cons '+ c))) e k)]
    [(state (list-rest (sub E1 E2) c) e k)              (state (cons E1 (cons E2 (cons '- c))) e k)]
    [(state (list-rest (mul E1 E2) c) e k)              (state (cons E1 (cons E2 (cons '* c))) e k)]
    
    [(state (list-rest '+ c) e (cons n2 (cons n1 k)))   (state c e (cons (+ n1 n2) k))]
    [(state (list-rest '- c) e (cons n2 (cons n1 k)))   (state c e (cons (- n1 n2) k))]
    [(state (list-rest '* c) e (cons n2 (cons n1 k)))   (state c e (cons (* n1 n2) k))]
    
    ;; boolean operations
    [(state (list-rest (¬ B) c) e k)                    (state (cons B (cons '¬ c)) e k)]
    [(state (list-rest (∧ B1 B2) c) e k)                (state (cons B1 (cons B2 (cons '∧ c))) e k)]

    [(state (list-rest '¬ c) e (cons b k))              (state c e (cons (not b) k))]
    [(state (list-rest '∧ c) e (cons b2 (cons b1 k)))   (state c e (cons (and b1 b2) k))]

    [(state (list-rest (=? E1 E2) c) e k)               (state (cons E1 (cons E2 (cons '=? c))) e k)]
    [(state (list-rest (≤ E1 E2) c) e k)                (state (cons E1 (cons E2 (cons '≤ c))) e k)]
    
    [(state (list-rest '=? c) e (cons n2 (cons n1 k)))  (state c e (cons (= n1 n2) k))]
    [(state (list-rest '≤ c) e (cons n2 (cons n1 k)))   (state c e (cons (<= n1 n2) k))]

    ;; commands
    [(state (list-rest (skip) c) e k)                   (state c e k)]
    
    [(state (list-rest (: C1 C2) c) e k)                (state (cons C1 (cons C2 c)) e k)]

    [(state (list-rest (:= x E) c) e k)                 (state (cons E (cons ':= c)) e (cons x k))]
    [(state (list-rest ':= c) e (cons n (cons x k)))    (state c (set-env e 't1 x n) k)]

    [(state (list-rest (ite B C1 C2) c) e k)                       (state (cons B (cons 'ite c)) e (cons C1 (cons C2 k)))]
    [(state (list-rest 'ite c) e (cons #t (cons C1 (cons C2 k))))  (state (cons C1 c) e k)]
    [(state (list-rest 'ite c) e (cons #f (cons C1 (cons C2 k))))  (state (cons C2 c) e k)]

    [(state (list-rest (while B C) c) e k)                         (state (cons B (cons 'while c)) e (cons B (cons C k)))]
    [(state (list-rest 'while c) e (cons #t (cons B (cons C k))))  (state (cons C (cons (while B C) c)) e k)]
    [(state (list-rest 'while c) e (cons #f (cons B (cons C k))))  (state c e k)]
    
    ;; todo load / store (global data)
    
    [(state (list-rest (? symbol? x) c) e k)	        (state c e (cons (lookup-env e 't1 x) k))] 
    [(state (list-rest (止) _) e k)                    (state '() e k)] 
))

(define t1-env
  (hash-set (hash) 't1 (hash)))

;(step (state '((止)) t1-env (list))) 

;(define p1 '(1 (止)))
;(step (step (state p1 t1-env (list))))

;(define p2 '( (add 1 2) ))
;(define s (state p2 t1-env (list)))
;(step s)
;(step (step s))
;(step (step (step s)))
;(step (step (step (step s))))

(define (run s)
  (begin
    (display s)
    (display "\n-->\n")
    (let [(next (step s))]
      (match next
        [(state (cons (止) _) _ (list))  ;; done when state is < 止, e, () >
         (display next)
         (display "\ndone\n\n")]
        [else  (run next)]))))

;(run (state (list 'a (止)) (set-env t1-env 't1 'a 6) (list)))

;(run (state (list (add 'a 4) (止)) (set-env t1-env 't1 'a 6) (list)))

;(run (state (list (:= 'a (mul (sub 6 4) 3)) (止)) t1-env (list)))

;(run (state (list (≤ (mul (sub 6 4) 3) (add -1 -2)) (止)) t1-env (list)))

;(run (state (list (=? (mul -1 3) (add -1 -2)) (止)) t1-env (list)))

;(run (state (list (¬ (=? (mul -1 3) (add -1 -2))) (止)) t1-env (list)))

;(run (state (list (∧ (≤ (add -1 -2) (mul (sub 6 4) 3)) (=? (mul -1 3) (add -1 -2))) (止)) t1-env (list)))

;(run (state (list (:= 'a 1) (止) ) t1-env (list)))

;(run (state (list (:= 'a 1) (止) ) (set-env t1-env 't1 'a 6) (list)))

;(run (state (list (: (:= 'a 1) (:= 'b (add 'a 10))) (止)) t1-env (list)))
;; do we need follows? :? yes, because if/while body
;(run (state (list (:= 'a 1) (skip) (:= 'b (add 'a 10)) (止)) t1-env (list)))

;(run (state (list (ite (≤ (mul (sub 6 4) 3) (add -1 -2)) (:= 'a 0) (:= 'a -1)) (止)) t1-env (list)))

(run (state (list 
                  (: (:= 'a 2)
                  (: (ite (=? 'a 2)
                          (: (:= 'a (add 'a 1))
                             (:= 'a (add 'a 1)))
                          (:= 'b 2))
                      (止))))
                   t1-env (list)))

(run (state (list 
                  (: (:= 'a 3)
                  (: (ite (=? 'a 2)
                          (: (:= 'a (add 'a 1))
                             (:= 'a (add 'a 1)))
                          (:= 'b 2))
                      (止))))
                   t1-env (list)))

(run (state (list
             (: (:= 'a 5)
             (: (:= 'b 50)
             (: (while (≤ 'a 'b)
                       (: (:= 'a (add 'a 1))
                          (:= 'b (sub 'b 1))))
                (止)))))
            t1-env (list)))
