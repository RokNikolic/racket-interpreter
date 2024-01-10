#lang racket

(provide (all-defined-out))

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

(define (reverse list_in)
    (if (null? list_in)
        null
        (append (reverse (cdr list_in)) (list (car list_in)))))

(define (remove x list_in)
    (cond 
        [(null? list_in) null]
        [(equal? x (car list_in)) (append null (remove x (cdr list_in)))]
        [#t (append (list (car list_in)) (remove x (cdr list_in)))]))

(define (map f list_in)
    (cond 
        [(null? list_in) null]
        [#t (append (list (f (car list_in))) (map f (cdr list_in)))]))

(define (filter f list_in)
    (cond 
        [(null? list_in) null]
        [(f (car list_in)) (append (list (car list_in)) (filter f (cdr list_in)))]
        [#t (filter f (cdr list_in))]))

(define (zip list1 list2)
    (cond 
        [(or (null? list1) (null? list2)) null]
        [#t (append (list (cons (car list1) (car list2))) (zip (cdr list1) (cdr list2)))]))

(define (range start stop step)
    (cond 
    [(> start stop) null]
    [#t (append (list start) (range (+ start step) stop step))]))

(define (is-palindrome list_in)
    (define (reverse list_r)
        (cond 
            [(null? list_r) null]
            [#t (append (reverse (cdr list_r)) (list (car list_r)))]))
    (equal? list_in (reverse list_in)))
