#lang racket

(define (power x n)
    (if (= n 0)
        1
        (* x (power x (- n 1)))))

(define (gcd a b)
    (if (not (= b 0))
        (gcd b (modulo a b))
        a))

(define (fib x)
    (define (fib_helper f1 f2 n)
        (cond 
            [(= n x) (+ f1 f2)]
            [#t (fib_helper f2 (+ f1 f2) (+ n 1))]))
    (cond 
        [(= x 1) 1]
        [(= x 2) 1]
        [#t (fib_helper 1 1 3)]))      

