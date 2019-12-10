#lang racket

;; how do it know??

(define (fib-func)
  (let ((prev 0)
        (cur 1))
    (define (loop)
      (define next (+ prev cur))
      (set! prev cur)
      (set! cur next)
      prev)
    loop))

(define test (fib-func))
(test)
(test)
(test)
(test)
(test)
