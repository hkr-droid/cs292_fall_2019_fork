
#lang racket

; arithmetic
(struct plus	(left right)	#:transparent)
(struct mul	(left right)	#:transparent)
(struct square	(arg)	        #:transparent)
(struct id	(str_key)	#:transparent)

; booleans
(struct top     ())
(struct bottom	())
(struct eq	(a0 a1))
(struct leq	(a0 a1))
(struct non	(b))
(struct et	(b0 b1))
(struct ou	(b0 b1))

; commands
(struct skip	())
(struct assign	(var val))
(struct ite	(b c0 c1))
(struct follows	(c0 c1))
(struct while	(b c))

(define (update-hash-table d var val)
  (hash-set! d var val)
  d ; return value
  )

; todo
; numeric wrapper type for naturals?
(define (interpret p st)
  (match p
    [(id x)	  (hash-ref st x)]
    [(plus a b)	  (+ (interpret a st)	     (interpret b st))]
    [(mul a b)	  (* (interpret a st)	     (interpret b st))]
    [(square a)	  (expt (interpret a st)		2)]
    
    [(top) #t]
    [(bottom) #f]
    [(eq a0 a1)	  (= (interpret a0 st)       (interpret a1 st))]
    [(leq a0 a1)  (<= (interpret a0 st)      (interpret a1 st))]
    [(non b)	  (not (interpret b st))]
    [(et b0 b1)	  (and (interpret b0 st))    (interpret b1 st)]
    [(ou b0 b1)	  (or (interpret b0 st)	     (interpret b1 st))]
  
    [(skip) st]
    [(assign var val)     (update-hash-table st var (interpret val st))]
    [(ite b c0 c1)   (if (interpret b st) (interpret c0 st) (interpret c1 st))]
    [(follows c0 c1) (let ([st1 (interpret c0 st)]) (interpret c1 st1))]

    ; allows non-terminating functions, could unroll only finitely many times
    [(while b c)   (if (interpret b st)
                       (interpret (follows c (while b c)) st)
                       st)] 

    [_ p]))


;; main
(define d
  (make-hash))
(hash-set! d "a0" 2) 
(hash-set! d "a1" 7)
(hash-set! d "a2" 3)


(define prog0  (plus (id "a0")	(id "a1")))
(define prog1  (non (top)))
(define prog2  (assign "a3" 18))
(define prog3  (skip))
(define prog4  (ite (top) (assign "a4" 16) (skip)))
(define prog5  (follows (assign "a3" 18) prog4))

(define prog6  (while (leq (id "a0") 5) (assign "a0" (plus (id "a0") 1))))

(define prog7  (leq (id "a0") 5))
(define prog8  (assign "a0" (plus (id "a0") 4)))


(define res (interpret prog6 d))
(println res)
