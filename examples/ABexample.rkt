#lang racket

(define A 5)
(define B 50)

(define lock-sem (make-semaphore 1))

(define thread1
  (thread (lambda ()
            (printf "thread 1: ~a,~a~n" A B)
            (define t1 A)
            (set! t1 (+ t1 1))
            (set! A t1)
            (printf "thread 1: ~a,~a~n" A B)
            (semaphore-wait lock-sem)
            (define t2 A)
            (set! t2 (+ t2 1))
            (set! A t2)
            (printf "thread 1: ~a,~a~n" A B)
            (define t3 B)
            (set! t3 (+ t3 1))
            (set! B t3)
            (printf "thread 1: ~a,~a~n" A B)
            (semaphore-post lock-sem)
            )))

(define thread2
  (thread (lambda ()
            (printf "thread 2: ~a,~a~n" A B)
            (semaphore-wait lock-sem)
            (define t1 A)
            (set! t1 (+ t1 1))
            (set! A t1)
            (printf "thread 2: ~a,~a~n" A B)
            (define t2 B)
            (set! t2 (+ t2 1))
            (set! B t2)
            (printf "thread 2: ~a,~a~n" A B)
            (semaphore-post lock-sem)
            )))

(for-each thread-wait (list thread1 thread2))
(printf "~nFinal: ~a,~a~n" A B)