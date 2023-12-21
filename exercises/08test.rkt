#lang racket

(require rackunit "08.rkt")
(require rackunit/text-ui)
 
(define file-tests
    (test-suite "Tests for 08.rkt"
    
    (check-equal? (fri (int 5)) (int 5) "int")

    (check-equal? (fri (false)) (false) "bool")

    (check-equal? (fri (add (int 3) (int 2))) (int 5) "add")
    (check-equal? (fri (add (false) (true))) (true) "add")
    (check-equal? (fri (add (int 5) (int 7))) (int 12) "add")
    (check-equal? (fri (add (int -3) (int 8))) (int 5) "add")
    (check-equal? (fri (add (int 0) (int 0))) (int 0) "add")
    (check-equal? (fri (add (false) (false))) (false) "add")

    (check-equal? (fri (mul (int 3) (int 2))) (int 6) "mul")
    (check-equal? (fri (mul (false) (true))) (false) "mul")
    (check-equal? (fri (mul (int 5) (int 7))) (int 35) "mul")
    (check-equal? (fri (mul (int -3) (int 8))) (int -24) "mul")
    (check-equal? (fri (mul (int 0) (int 0))) (int 0) "mul")
    (check-equal? (fri (mul (true) (true))) (true) "mul")

    (check-equal? (fri (?leq (int 3) (int 2))) (false) "?leq")
    (check-equal? (fri (?leq (int 3) (int 3))) (true) "?leq")
    (check-equal? (fri (?leq (int 2) (int 3))) (true) "?leq")
    (check-equal? (fri (?leq (true) (true))) (true) "?leq")
    (check-equal? (fri (?leq (true) (false))) (false) "?leq")
    (check-equal? (fri (?leq (false) (true))) (true) "?leq")
    (check-equal? (fri (?leq (false) (false))) (true) "?leq")

    (check-equal? (fri (~ (int 3))) (int -3) "neg")
    (check-equal? (fri (~ (false))) (true) "neg")

    (check-equal? (fri (?int (int 5))) (true) "?int")

    (check-equal? (fri (if-then-else (true) (int 5) (add (int 2) (int "a")))) (int 5) "if-then-else")

    (check-equal? (conditional (true) (int -100) (mul (true) (false)) (add (int 1) (int 1)) (int 9000)) (if-then-else (true) (int -100) (if-then-else (mul (true) (false)) (add (int 1) (int 1)) (int 9000))) "conditional")

    (check-equal? (?geq (add (int 1) (int 1)) (int 4)) (?leq (int 4) (add (int 1) (int 1))) "?geq")

))

(run-tests file-tests)