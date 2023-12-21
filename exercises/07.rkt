#lang racket

(provide (all-defined-out))

(define ones (cons 1 (λ () ones)))
(define twos (cons 2 (thunk twos)))

; -------------------------------- 
(define naturals 
    (letrec ([gen_nat (lambda (x) (cons x (thunk (gen_nat (+ x 1)))))])
        (gen_nat 1)))

(define gen_naturals (lambda (x) (cons x (thunk (gen_naturals (+ x 1))))))
(define naturals_different (gen_naturals 1))
; -------------------------------- 
; -------------------------------- 
(define fibs 
    (letrec ([gen_fib (lambda (f1 f2) (cons f1 (thunk (gen_fib f2 (+ f1 f2)))))])
    (gen_fib 1 1)))

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

(define (stream_pow n stream)
    (cons (expt (car stream) n) (thunk (stream_pow n ((cdr stream))))))

(define/match (zip s1 s2)
    [((cons h1 t1) (cons h2 t2))
        (cons h1 (thunk (cons h2 (thunk (zip (t1) (t2))))))])

(struct some (a) #:transparent)
(struct none () #:transparent)
(define-syntax sml
    (syntax-rules (valOf isSome SOME NONE nil null hd tl ::)
        [(sml NONE) (none)]
        [(sml SOME a) (some a)]
        [(sml valOf a) (some-a a)]
        [(sml isSome a) (some? a)]

        [(sml nil) null]
        [(sml null list_in) (null? list_in)]
        [(sml hd list_in) (car list_in)]
        [(sml tl list_in) (cdr list_in)]
        [(sml element :: list_in) (append (list element) list_in)]))

; this one is still needs fixing
(define (partitions k n)
    (* k n))

; function with any number of args
(define (pair a b . ostali)
    (if (null? ostali) 
        (list (cons a b))
        (cons (cons a b) (apply pair ostali))))

; from list to stream
(define (stream 1st)
    (let loop ([xs 1st])
        (match xs
            ['() (loop 1st)]
            [(cons h t) (cons h (thunk (loop t)))])))