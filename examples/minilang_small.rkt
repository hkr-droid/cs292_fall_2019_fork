
#lang racket
(require racket/match racket/struct)

;; env is a map of maps
;; given a tag, look up the map for that store 
;; (e.g. 'thread1, 'global, 'semaphore);; then return the value for the given key

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
(struct load (g x) #:transparent)
(struct store (x g) #:transparent)

(struct 止 () #:transparent)  ;; as in 止まれ (tomare), halt
(struct par (c0 c1) #:transparent)

(struct EvalArg (c e s k) #:transparent)
(struct clos (c e) #:transparent)

;; state struct with "pretty" printing
(struct state (c e s k)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'state)
      (lambda (obj) (list (state-c obj) (state-e obj) (state-s obj) (state-k obj)))))])

;; step function
(define (step s)
  (match s

    ;; values
    [(state (list-rest (? number? n) c) e s k)            (state c e s (cons n k))]
    [(state (list-rest (? boolean? b) c) e s k)           (state c e s (cons b k))]

    ;; arithmetic operations
    [(state (list-rest (add E1 E2) c) e s k)              (state (cons E1 (cons E2 (cons '+ c))) e s k)]
    [(state (list-rest (sub E1 E2) c) e s k)              (state (cons E1 (cons E2 (cons '- c))) e s k)]
    [(state (list-rest (mul E1 E2) c) e s k)              (state (cons E1 (cons E2 (cons '* c))) e s k)]
    
    [(state (list-rest '+ c) e s (cons n2 (cons n1 k)))   (state c e s (cons (+ n1 n2) k))]
    [(state (list-rest '- c) e s (cons n2 (cons n1 k)))   (state c e s (cons (- n1 n2) k))]
    [(state (list-rest '* c) e s (cons n2 (cons n1 k)))   (state c e s (cons (* n1 n2) k))]   

    ;; boolean operations
    [(state (list-rest (¬ B) c) e s k)                    (state (cons B (cons '¬ c)) e s k)]
    [(state (list-rest (∧ B1 B2) c) e s k)                (state (cons B1 (cons B2 (cons '∧ c))) e s k)]
    [(state (list-rest '¬ c) e s (cons b k))              (state c e s (cons (not b) k))]
    [(state (list-rest '∧ c) e s (cons b2 (cons b1 k)))   (state c e s (cons (and b1 b2) k))]
    [(state (list-rest (=? E1 E2) c) e s k)               (state (cons E1 (cons E2 (cons '=? c))) e s k)]
    [(state (list-rest (≤ E1 E2) c) e s k)                (state (cons E1 (cons E2 (cons '≤ c))) e s k)]
    
    [(state (list-rest '=? c) e s (cons n2 (cons n1 k)))  (state c e s (cons (= n1 n2) k))]
    [(state (list-rest '≤ c) e s (cons n2 (cons n1 k)))   (state c e s (cons (<= n1 n2) k))]

    ;; commands
    [(state (list-rest (skip) c) e s k)                   (state c e s k)]

    ;; sequence commands C1 ; C2
    [(state (list-rest (: C1 C2) c) e s k)                (state (cons C1 (cons C2 c)) e s k)]

    ;; assign, x := E
    [(state (list-rest (:= x E) c) e s k)                 (state (cons E (cons ':= c)) e s (cons x k))]
    [(state (list-rest ':= c) e s (cons n (cons x k)))    (state c (set-env e s x n) s k)]
    ;;[(state (list-rest ':= c) e s (cons n (cons x k)))    (display "made it here\n") (state (clos c e) (set-env e s x n) s k)]

    ;; if B then C1 else C2
    [(state (list-rest (ite B C1 C2) c) e s k)                       (state (cons B (cons 'ite c)) e s (cons C1 (cons C2 k)))]
    [(state (list-rest 'ite c) e s (cons #t (cons C1 (cons C2 k))))  (state (cons C1 c) e s k)]
    [(state (list-rest 'ite c) e s (cons #f (cons C1 (cons C2 k))))  (state (cons C2 c) e s k)]

    ;; while B do C
    [(state (list-rest (while B C) c) e s k)                         (state (cons B (cons 'while c)) e s (cons B (cons C k)))]
    [(state (list-rest 'while c) e s (cons #t (cons B (cons C k))))  (state (cons C (cons (while B C) c)) e s k)]
    [(state (list-rest 'while c) e s (cons #f (cons B (cons C k))))  (state c e s k)]    

    ;; load / store (global data)
    [(state (list-rest (load g x) c) e s k)               (state (cons g (cons 'load c)) e 'global (cons x k))]
    [(state (list-rest 'load c) e s (cons n (cons x k)))  (state c (set-env e s x n) s k)]
    [(state (list-rest (store x g) c) e s k)              (state (cons x (cons 'store c)) e s (cons g k))]
    [(state (list-rest 'store c) e s (cons n (cons g k))) (state c (set-env e 'global g n) s k)]

    ;; parallel operator
    ;; choose a command randomly, left or right, to execute as the control 
    ;; string, then store the rest of the command as a continuation
    [(state (list-rest (par c0 c1) c) e s k)                            (let* ([target (list-ref (list c0 c1) (random 2))]
                                                                               [other (car (remove target (list c0 c1)))]
                                                                               [hole (gensym "hole")])
                                                                          (state (list target) e s
                                                                                 (EvalArg (cons (par hole other) c) e s k))
                                                                          )]

    ;; another "indirection": one, single evaluation step of a command returns
    ;; a closure, we wait for that, then check if we need to pop a continuation
    [(state '() e1 s1 (EvalArg (list-rest (par hole c2)  c) e2 s2 k))       (state (list c2) e1 s1 k)]
    [(state c e1 s (list-rest (EvalArg (par hole c2) e2 s k1) k2))          (state (par c c2) e1 s k2)]
    
    ;; atomic region
    ;; < (atomic (list c0 c1) . c) e s k > --> < (c0 . atomic c1 . c) e s k


    ;; look up symbol x in e(s(x))
    [(state (list-rest (? symbol? x) c) e s k)            (state c e s (cons (lookup-env e s x) k))]

    ;; special halt terminal
    [(state (list-rest (止) _) e s k)                     (state '() e s k)] 

))

(define (run s)
  (begin
    (display s)
    (display "\n-->\n")
    (let [(next (step s))]
      (match next
        [(state (cons (止) _) _ _ (list))  ;; done when state is < 止, e, s, () >
         (display next)
         (display "\ndone\n\n")]
        [else  (run next)]))))
(define t1-env
  (hash-set (hash) 't1 (hash)))

;; ;; assign test
;; (run (state (list (: (:= 'a 1) (:= 'b (add 'a 10))) (止)) t1-env 't1 (list)))
;; ;; ite true branch
;; (run (state (list 
;;                   (: (:= 'a 2)
;;                   (: (ite (=? 'a 2)
;;                           (: (:= 'a (add 'a 1))
;;                              (:= 'a (add 'a 1)))
;;                           (:= 'b 2))
;;                       (止))))
;;                    t1-env 't1 (list)))

;; ;; ite false branch
;; (run (state (list 
;;                   (: (:= 'a 3)
;;                   (: (ite (=? 'a 2)
;;                           (: (:= 'a (add 'a 1))
;;                              (:= 'a (add 'a 1)))
;;                           (:= 'b 2))
;;                       (止))))
;;                    t1-env 't1 (list)))

;; while test (commented out cause print output is long)

;(run (state (list
;             (: (:= 'a 5)
;             (: (:= 'b 50)
;             (: (while (≤ 'a 'b)
;                       (: (:= 'a (add 'a 1))
;                          (:= 'b (sub 'b 1))))
;                (止)))))
;            t1-env 't1 (list)))

(define global-env
  (hash-set (hash-set (hash) 'A 5) 'B 50))

(define test-env
  (hash-set (hash-set (hash) 't1 (hash)) 'global global-env))

;; possible answers are 1, 4, 5
(run (state (list
             (:
              (par
               (:
                (:= 'a 2)
                (止))
               (:
                (:= 'a 1)
                (止))
               )
              (止))
             )
            test-env 't1 (list)))

;; (run (state (list
;;              (:
;;               (:= 'a 2)
;;               (止)))
;;             test-env 't1 (list)))

;; ;; load/store test
;; (run (state (list
;;              (: (load 'A 'a)
;;                 (: (:= 'a (add 'a 1))
;;                    (: (store 'a 'A)
;;                       (止)
;;                       )
;;                    )
;;                 )
;;              )
;;             test-env 't1 (list)))

