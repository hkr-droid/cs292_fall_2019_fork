#lang racket

(require racket/match racket/struct)

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
(struct 止 () #:transparent)  ;; as in 止まれ (tomare), halt

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
    [(state (cons (? boolean? b) c) e k)        (state c e (cons b k))]
    
    ;[(?symbol? e k)	                         (state (env-lookup ?symbol? e) e k)]  ;; todo ensure lookup works

    ;; arithmetic operations
    [(state (cons `(add ,E1 ,E2) c) e k)            (state (cons E1 (cons E2 (cons '+ c))) e k)]
    [(state (cons `(sub ,E1 ,E2) c) e k)            (state (cons E1 (cons E2 (cons '- c))) e k)]
    [(state (cons `(mul ,E1 ,E2) c) e k)            (state (cons E1 (cons E2 (cons '* c))) e k)]
    
    [(state (cons '+ c) e (cons n2 (cons n1 k)))   (state c e (cons (+ n1 n2) k))]
    [(state (cons '- c) e (cons n2 (cons n1 k)))   (state c e (cons (- n1 n2) k))]
    [(state (cons '* c) e (cons n2 (cons n1 k)))   (state c e (cons (* n1 n2) k))]
    
    ;; boolean operations
    [(state (cons `(¬ ,B) c) e k)                   (state (cons B (cons '¬ c)) e k)]
    [(state (cons `(∧ ,B1 ,B2) c) e k)              (state (cons B1 (cons B2 (cons '∧ c))) e k)]

    [(state (cons '¬ c) e (cons b k))                (state c e (cons (not b) k))]
    [(state (cons '∧ c) e (cons b2 (cons b1 k)))     (state c e (cons (and b1 b2) k))]

    [(state (cons `(=? ,E1 ,E2) c) e k)             (state (cons E1 (cons E2 (cons '=? c))) e k)]
    [(state (cons `(≤ ,E1 ,E2) c) e k)              (state (cons E1 (cons E2 (cons '≤ c))) e k)]
    
    [(state (cons '=? c) e (cons n2 (cons n1 k)))    (state c e (cons (= n1 n2) k))]
    [(state (cons '≤ c) e (cons n2 (cons n1 k)))     (state c e (cons (<= n1 n2) k))]

    [(state (cons `(skip) c) e k)                   (state c e k)]
    
    [(state (cons `(: ,C1 ,C2) c) e k)              (state (cons C1 (cons C2 c)) e k)]
    
    
    ;[((list-ref skip c) e k)                          (state c e k)
    ;[((list-ref (ite b c0 c1) c) e k)                 (state (cons b (cons ite c)) e (cons c0 (cons c1 k)))]
    ;[((list-ref ite c) e (list-ref #t c0 c1 k) )    (state c0 e k)]
    ;[((list-ref ite c) e (list-ref #f c0 c1 k) )   (state c1 e k)]
    ;[((list-ref follows c0 c1 c) e k)	              (state (cons c0 (cons c1 c)) e k)]
    ;; todo [(assign ,v ,e)	          (env-set! env v e)]
    ;; todo while

    [(state (cons `(止) _) e k)                  (state '() e k)] 
))

(step (state '((止)) (list) (list))) 

(define p1 '(1 (止)))
(step (step (state p1 (list) (list))))

(define p2 '( (add 1 2) ))
(define s (state p2 (list) (list)))
(step s)
(step (step s))
(step (step (step s)))
(step (step (step (step s))))

(define (run s)
  (begin
    (display s)
    (display "\n-->\n")
    (let [(next (step s))]
      (match next
        [(state (cons '(止) _) _ _)
         (display next)
         (display "\ndone\n")]
        [else  (run next)]))))

(run (state '( (sub 6 4) (止)) (list) (list)))

(run (state '( (mul (sub 6 4) 3) (止)) (list) (list)))

(run (state '( (≤ (mul (sub 6 4) 3) (add -1 -2)) (止)) (list) (list)))

(run (state '( (=? (mul -1 3) (add -1 -2)) (止)) (list) (list)))

(run (state '( (¬ (=? (mul -1 3) (add -1 -2))) (止)) (list) (list)))

(run (state '( (∧ (≤ (add -1 -2) (mul (sub 6 4) 3)) (=? (mul -1 3) (add -1 -2))) (止)) (list) (list)))

(run (state '( (skip) (sub 6 4) (止)) (list) (list)))

(run (state '( (: (skip) (sub 6 4)) (止)) (list) (list)))