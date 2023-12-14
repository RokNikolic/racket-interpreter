#lang racket

(require rackunit "06.rkt")
(require rackunit/text-ui)
 
(define file-tests
    (test-suite "Tests for 06.rkt"
    
    (check-equal? (power 2 3) 8 "power")
    (check-equal? (power 2 0) 1 "power")
    (check-equal? (power 2 10) 1024 "power")
    (check-equal? (power 1 10) 1 "power")
    (check-equal? (power 10 2) 100 "power")

    (check-equal? (gcd 7 3) 1 "gcd")
    (check-equal? (gcd 8 6) 2 "gcd")
    (check-equal? (gcd 54 24) 6 "gcd")
    (check-equal? (gcd 999 1) 1 "gcd")
    (check-equal? (gcd 1337 1337) 1337 "gcd")

    (check-equal? (fib 1) 1 "fib")
    (check-equal? (fib 3) 2 "fib")
    (check-equal? (fib 10) 55 "fib")
    (check-equal? (fib 50) 12586269025 "fib")
    (check-equal? (fib 1000) 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875  "fib")

    (check-equal? (reverse (list)) '() "reverse")
    (check-equal? (reverse (list 1)) '(1) "reverse")
    (check-equal? (reverse (list "1" "2")) '("2" "1") "reverse")
    (check-equal? (reverse (list 1 2 3)) '(3 2 1) "reverse")
    
    (check-equal? (remove 3 (list 1 2 3 4 5 4 3 2 1)) '(1 2 4 5 4 2 1) "remove")
    (check-equal? (remove 99 (list 1 2 3 4 5 4 3 2 1)) '(1 2 3 4 5 4 3 2 1) "remove")
    (check-equal? (remove "3" (list "1" "2" "3" "4" "5" "4" "3" "2" "1")) '("1" "2" "4" "5" "4" "2" "1") "remove")
    
    (check-equal? (map (lambda (a) (* a 2)) (list 1 2 3)) '(2 4 6) "map")
    (check-equal? (map (lambda (a) (string-append a " banana")) (list "a" "amazing")) '("a banana" "amazing banana") "map")

    (check-equal? (filter (lambda (a) (= (modulo a 2) 0)) (list 1 2 3)) '(2) "filter")
    (check-equal? (filter (lambda (a) (= a 0)) (list 1 2 3 4 5)) '() "filter")
    (check-equal? (filter (lambda (a) (> a 0)) (list 1 2 3 4 5)) '(1 2 3 4 5) "filter")
    (check-equal? (filter (lambda (a) (= a 999)) (list)) '())
    
    (check-equal? (zip (list 1 2 3) (list 4 5 6)) '((1 . 4) (2 . 5) (3 . 6)) "zip")
    (check-equal? (zip (list 1 2 3 9) (list 4 5 6)) '((1 . 4) (2 . 5) (3 . 6)) "zip")
    (check-equal? (zip (list 1 2 3 9) (list 4 5 6 9 9)) '((1 . 4) (2 . 5) (3 . 6) (9 . 9)) "zip")

    (check-equal? (range 1 3 1) '(1 2 3) "range")
    (check-equal? (range 1 3 2) '(1 3) "range")
    (check-equal? (range 1 3 3) '(1) "range")
    (check-equal? (range 10 30 1.5) '(10 11.5 13.0 14.5 16.0 17.5 19.0 20.5 22.0 23.5 25.0 26.5 28.0 29.5) "range")
    
    (check-equal? (is-palindrome (list 2 3 5 1 6 1 5 3 2)) #t "is-palindrome")
    (check-equal? (is-palindrome (list 1 2 3 2 1)) #t "is-palindrome")
    (check-equal? (is-palindrome (list 1 2 3 2 1 2)) #f "is-palindrome")
    (check-equal? (is-palindrome (list 1)) #t "is-palindrome")
    (check-equal? (is-palindrome (list)) #t "is-palindrome")
))

(run-tests file-tests)
