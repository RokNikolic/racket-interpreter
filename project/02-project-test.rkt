#lang racket

(require rackunit "02-project.rkt")
(require rackunit/text-ui)
 
(define file-tests
    (test-suite "Tests for 02-project.rkt"
    
    (check-equal? (fri (int 5) null) (int 5) "int")

    (check-equal? (fri (false) null) (false) "bool")

    (check-equal? (fri (add (int 3) (int 2)) null) (int 5) "add")
    (check-equal? (fri (add (false) (true)) null) (true) "add")
    (check-equal? (fri (add (int 5) (int 7)) null) (int 12) "add")
    (check-equal? (fri (add (int -3) (int 8)) null) (int 5) "add")
    (check-equal? (fri (add (int 0) (int 0)) null) (int 0) "add")
    (check-equal? (fri (add (false) (false)) null) (false) "add")
    (check-equal? (add (mul (true) (true)) (false)) (add (mul (true) (true)) (false)))
    (check-equal? (fri (add (mul (true) (true)) (false)) null) (true))
    (check-equal? (fri (add (add (int 9) (int 9)) (true)) null) (triggered (exception "add: wrong argument type")))

    (check-equal? (fri (mul (int 3) (int 2)) null) (int 6) "mul")
    (check-equal? (fri (mul (false) (true)) null) (false) "mul")
    (check-equal? (fri (mul (int 5) (int 7)) null) (int 35) "mul")
    (check-equal? (fri (mul (int -3) (int 8)) null) (int -24) "mul")
    (check-equal? (fri (mul (int 0) (int 0)) null) (int 0) "mul")
    (check-equal? (fri (mul (true) (true)) null) (true) "mul")

    (check-equal? (fri (?leq (int 3) (int 2)) null) (false) "?leq")
    (check-equal? (fri (?leq (int 3) (int 3)) null) (true) "?leq")
    (check-equal? (fri (?leq (int 2) (int 3)) null) (true) "?leq")
    (check-equal? (fri (?leq (true) (true)) null) (true) "?leq")
    (check-equal? (fri (?leq (true) (false)) null) (false) "?leq")
    (check-equal? (fri (?leq (false) (true)) null) (true) "?leq")
    (check-equal? (fri (?leq (false) (false)) null) (true) "?leq")

    (check-equal? (fri (~ (int 3)) null) (int -3) "neg")
    (check-equal? (fri (~ (false)) null) (true) "neg")

    (check-equal? (fri (?int (int 5)) null) (true) "?int")

    (check-equal? (fri (if-then-else (true) (int 5) (add (int 2) (int "a"))) null) (int 5) "if-then-else")

))

(run-tests file-tests)