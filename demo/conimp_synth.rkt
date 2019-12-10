#lang rosette

(require racket/struct)
(require rosette/lib/match rosette/lib/angelic rosette/lib/synthax)

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
(struct load (g x) #:transparent)
(struct store (x g) #:transparent)
(struct atomic (c) #:transparent)
(struct 止 () #:transparent)  ;; as in 止まれ (tomare), halt

;; state struct with "pretty" printing
(struct state (c e s k)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'state)
      (lambda (obj) (list (state-c obj) (state-e obj) (state-s obj) (state-k obj)))))])

;; step function
(define (step st)
  (match st

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

    ;; if B then C1 else C2
    [(state (list-rest (ite B C1 C2) c) e s k)                       (state (cons B (cons 'ite c)) e s (cons C1 (cons C2 k)))]
    [(state (list-rest 'ite c) e s (cons #t (cons C1 (cons C2 k))))  (state (cons C1 c) e s k)]
    [(state (list-rest 'ite c) e s (cons #f (cons C1 (cons C2 k))))  (state (cons C2 c) e s k)]

    ;; while B do C
    [(state (list-rest (while B C) c) e s k)                         (state (cons B (cons 'while c)) e s (cons B (cons C k)))]
    [(state (list-rest 'while c) e s (cons #t (cons B (cons C k))))  (state (cons C (cons (while B C) c)) e s k)]
    [(state (list-rest 'while c) e s (cons #f (cons B (cons C k))))  (state c e s k)]
    
    ;; load / store (global data)
    [(state (list-rest (load g x) c) e s k)               (state (cons (lookup-env e 'global g) (cons 'load c)) e s (cons x k))] ;; cheater!
    [(state (list-rest 'load c) e s (cons n (cons x k)))  (state c (set-env e s x n) s k)]
    [(state (list-rest (store x g) c) e s k)              (state (cons x (cons 'store c)) e s (cons g k))]
    [(state (list-rest 'store c) e s (cons n (cons g k))) (state c (set-env e 'global g n) s k)]

    ;; look up symbol x in e(s(x))
    [(state (list-rest (? symbol? x) c) e s k)	          (state c e s (cons (lookup-env e s x) k))]
    ;; special halt terminal
    [(state (list-rest (止) _) e s k)                     (state (list) e s k)]

    [else st] ;; problem?
))

;; sequential run
(define (run s)
  (begin
    (display s)
    (display "\n-->\n")
    (let [(next (step s))]
      (match next
        [(state (cons (止) _) _ _ (list))  ;; done when state is < 止, e, s, () >
         (display next)
         (display "\ndone\n\n")
         ]
        [else  (run next)]))))

;; sequential tests
;(define t1-env
;  (hash-set (hash) 't1 (hash)))

;; assign test
;(run (state (list (: (:= 'a 1) (:= 'b (add 'a 10))) (止)) t1-env 't1 (list)))

;; ite true branch
;(run (state (list 
;                  (: (:= 'a 2)
;                  (: (ite (=? 'a 2)
;                          (: (:= 'a (add 'a 1))
;                             (:= 'a (add 'a 1)))
;                          (:= 'b 2))
;                      (止))))
;                   t1-env 't1 (list)))

;; ite false branch
;(run (state (list 
;                  (: (:= 'a 3)
;                  (: (ite (=? 'a 2)
;                          (: (:= 'a (add 'a 1))
;                             (:= 'a (add 'a 1)))
;                          (:= 'b 2))
;                      (止))))
;                   t1-env 't1 (list)))

;; while test (commented out cause print output is long)
;(run (state (list
;             (: (:= 'a 5)
;             (: (:= 'b 50)
;             (: (while (≤ 'a 'b)
;                       (: (:= 'a (add 'a 1))
;                          (:= 'b (sub 'b 1))))
;                (止)))))
;            t1-env 't1 (list)))


(define (parrun c e k)
  (begin
    (if (not (hash-empty? c))
    ;(if (not (all-halt c))
        (let*
            ([i (list-ref (hash-keys c) (random (hash-count c)))]
             [ci (hash-ref c i)]
             [ki (hash-ref k i)]
             [st (state ci e i ki)])

          (if (empty? ci)
              ;; remove thread
              (begin
                (display "removing ") (display i)
                (display "\n-->\n")
                (parrun (hash-remove c i)
                        e
                        (hash-remove k i)))
              ;; else -> step
              (begin     
                (define next #f)
          
                (match (car ci)
                  [(atomic ca)
                   (let atomic-exec ([as (step (state ca e i ki))])
                     (display as)
                     (display "\n-->\n")
                     (if (not (empty? (state-c as)))
                         (atomic-exec (step as))
                         (set! next as)))]
                  [else (set! next (step st))])
              
                (begin (display next)
                       (display "\n-->\n")
                       (parrun (hash-set c i (state-c next))
                               (state-e next)
                               (hash-set k i (state-k next)))))))
        (begin (display "done!\n\n") e)
)))


(define global-env
  (hash-set (hash-set (hash) 'A 5) 'B 50))
(define test-env
  (hash-set (hash-set (hash) 't1 (hash)) 'global global-env))

;; redoing tests from before to make sure I didn't break everything
;; ite false branch
;(parrun (hash-set (hash) 0
;                 (list 
;                  (: (:= 'a 3)
;                  (: (ite (=? 'a 2)
;                          (: (:= 'a (add 'a 1))
;                             (:= 'a (add 'a 1)))
;                          (:= 'b 2))
;                      (止)))))
;        (hash-set (hash) 0 (hash))
;        (hash-set (hash) 0 (list)))

;; ite true branch
;(parrun (hash-set (hash) 0
;                 (list 
;                  (: (:= 'a 2)
;                  (: (ite (=? 'a 2)
;                          (: (:= 'a (add 'a 1))
;                             (:= 'a (add 'a 1)))
;                          (:= 'b 2))
;                      (止))))
;                 )
;        (hash-set (hash) 0 (hash))
;        (hash-set (hash) 0 (list)))

;; assign test
;(parrun (hash-set (hash) 0
;                 (list
;                  (: (:= 'a 1)
;                     (:= 'b (add 'a 10)))
;                     (止))
;                 )
;        (hash-set (hash) 0 (hash))
;        (hash-set (hash) 0 (list)))

;; load/store test
;(parrun (hash-set (hash) 0
;                 (list
;                  (: (load 'A 'a)
;                  (: (:= 'a (add 'a 1))
;                  (: (store 'a 'A)
;                     (止)))))
;                 )
;        (hash-set (hash-set (hash) 0 (hash)) 'global global-env)
;        (hash-set (hash) 0 (list)))

;(define C0 (list
;             (: (load 'A 'a)
;             (: (:= 'a (add 'a 1))
;             (: (store 'a 'A)
;                (止))))))

;(define C1 (list
;             (: (load 'B 'b)
;             (: (:= 'b (add 'b 1))
;             (: (store 'b 'B)
;             (: (load 'A 'a)
;             (: (:= 'a (add 'a 1))
;             (: (store 'a 'A)
;                (止)))))))))


(define test-k (hash-set (hash-set (hash) 0 (list)) 1 (list)))

(define-symbolic x y integer?)

(define (??expr terminals)
  (define a (apply choose* terminals))
  (define b (apply choose* terminals))
  (choose* (add a b)
           (sub a b)
           a))

(define A0? (list
             (: (load 'A 'a)
             (: (:= 'a (add 'a 1))
             (: (store 'a 'A)
             (: (atomic
                 (list (: (load 'A 'a)
                       (: (:= 'a (add 'a (??)))
                       ;(: (:= 'a (??expr (list x y)))
                       (: (store 'a 'A)
                       (: (load 'B 'b)
                       (: (:= 'b (add 'b 1))
                          (store 'b 'B))))))))
                (止)))))))

(define A1? (list
             (: (atomic
                 (list (: (load 'A 'a)
                       (: (:= 'a (add 'a 1))
                       (: (store 'a 'A)
                       (: (load 'B 'b)
                       (: (:= 'b (add 'b 1))
                          (store 'b 'B))))))))
                (止))))

(define binding
    (synthesize #:forall (list x y)
                #:guarantee (assert (let ([g (hash-ref (parrun (hash-set (hash-set (hash) 0 A0?) 1 A1?)
                                                         (hash-set (hash-set (hash-set (hash) 0 (hash)) 1 (hash))
                                                                   'global (hash-set (hash-set (hash) 'A x) 'B y))
                                                         test-k) 'global)])
                                (and (= (hash-ref g 'A) 7)
                                     (= (hash-ref g 'B) 52))))))

(print-forms binding)
