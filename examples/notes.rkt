
exp ::= (exp exp)           ; function application
      |  (lambda (var) exp)  ; anonymous function
      |  var                 ; variable reference

(define (cps-convert term cont)
  (match term
    [`(,f ,e)
     ; =>
     (let (($f (gensym 'f))
           ($e (gensym 'e)))
       (cps-convert f `(lambda (,$f)
         ,(cps-convert e `(lambda (,$e)
             (,$f ,$e ,cont))))))]
    
    [`(lambda (,v) ,e)
     ; =>
     (let (($k (gensym 'k)))
       `(,cont (lambda (,v ,$k)
                 ,(cps-convert e $k))))]
    
    [(? symbol?)
     ; =>
     `(,cont ,term)]))

(define (cps-convert-program term)
  (cps-convert term '(lambda (ans) ans)))

 call/cc => (lambda (f cc) (f (lambda (x k) (cc x)) cc))

;; This desugaring (in conjunction with the CPS transformation) is the best way to understand exactly what call/cc does.

;; It does exactly what it's name says it will: it calls the procedure given as an argument with a procedure that has captured the current continuation.

;; When that procedure capturing the continuation gets invoked, it "returns" the computation to the point at which the computation was created. 

(define (sum-interval a b cont)
  (if (= a b)
      (cont a)
      (sum-interval
       (+ a 1)
       b
       (lambda (x)
         (cont (+ a x))))))

(define (append l1 l2)
  (if (eq? l1 ’())
      l2
      (cons (car l1)
            (append (cdr l1) l2))))

(define (cs-append l1 l2 cont)
  (if (eq? l1 ’())
      (cont l2)
      (cs-append(cdr l1)l2
                (lambda (appended-cdr)
                  (cont (cons (car l1)
                              appended-cdr))))))


(define (flatten tree)
  (cond ((null? tree) ’())
        ((not (pair? tree)) (list tree))
        (else (append (flatten (car tree))
                      (flatten (cdr tree))))))

(define (cs-flatten tree cont)
  (cond ((null? tree) (cont ’()))
        ((not (pair? tree))         (cont (list tree)))
        (else (cs-flatten
               (car tree)
               (lambda (car-leaves)
                 (cs-flatten
                  (cdr tree)
                  (lambda (cdr-leaves)
                    (cont
                     (append car-leaves cdr-leaves)))))))))


(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (eq? input ’**quit**)
        ’c-eval-done
        (c-eval
         input
         the-global-environment
         (lambda (output)
           (announce-output output-prompt)
           (display output)
           (driver-loop))))))

(define (c-eval exp env cont)
  (cond ((self-evaluating? exp)
         (cont exp))
        ((variable? exp)(cont (lookup-variable-value exp env)))
        ((quoted? exp)(cont (text-of-quotation exp)))
        ((assignment? exp)(eval-assignment exp env cont))
        ((definition? exp)(eval-definition exp env cont))
        ((if? exp) (eval-if exp env cont))
        ((lambda? exp)(cont (make-procedure (lambda-parameters exp)(lambda-body exp) env)))
        ((begin? exp)(eval-sequence (begin-actions exp) env cont))
        ((cond? exp)(c-eval (cond->if exp) env cont))
        ))

(define (eval-if exp env cont)
  (c-eval(if-predicate exp) env
         (lambda (test-value)
           (if test-value(c-eval (if-consequent exp) env cont)
               (c-eval (if-alternative exp) env cont)))))

(define (eval-sequence exps env cont)
  (if (last-exp? exps)(c-eval (first-exp exps) env cont)
      (c-eval (first-exp exps) env
              (lambda (ignored)(eval-sequence(rest-exps exps)env cont)))))
