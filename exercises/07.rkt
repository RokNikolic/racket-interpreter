#lang racket

(provide (all-defined-out))

(define ones (cons 1 (λ () ones)))

(define twos (cons 2 (thunk twos)))

(define naturals 
    (letrec ([gen_nat (lambda (x) (cons x (thunk (gen_nat (+ x 1)))))])
        (gen_nat 1)))

; -------------------------------- 
(define gen_naturals (lambda (x) (cons x (thunk (gen_naturals (+ x 1))))))
(define naturals2 (gen_naturals 1))
(define (generate_nat x) (cons x (thunk (generate_nat (+ x 1)))))
(define naturals3 (generate_nat 1))
; --------------------------------   

(define fibs 
    (letrec ([gen_fib (lambda (f1 f2) (cons f1 (thunk (gen_fib f2 (+ f1 f2)))))])
    (gen_fib 1 1)))

; -------------------------------- 
(define (gen_fib_different f1 f2) (cons f1 (thunk (gen_fib_different f2 (+ f1 f2)))))
(define fibs_different (gen_fib_different 1 1))
; -------------------------------- 

(define (first n stream)
    (if (zero? n) 
        '()
        (cons (car stream) (first (- n 1) ((cdr stream))))))

(define (nth n stream)
    (if (zero? (- n 1))
        (car stream)
        (nth (- n 1) ((cdr stream)))))

(define (squares stream)
    (cons (expt (car stream) 2) (thunk (squares ((cdr stream))))))

; -------------------------------- 
(define (stream_pow n stream)
    (cons (expt (car stream) n) (thunk (stream_pow n ((cdr stream))))))
; -------------------------------- 
