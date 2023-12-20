#lang racket

(require rackunit "07.rkt")
(require rackunit/text-ui)
 
(define file-tests
    (test-suite "Tests for 07.rkt"
    
    (check-equal? (car ones) 1 "ones")
    (check-equal? (car twos) 2 "twos")
    (check-equal? (car ((cdr ((cdr ones))))) 1 "ones")
    (check-equal? (first 10 ones) '(1 1 1 1 1 1 1 1 1 1) "fib + first")
    (check-equal? (nth 1000 ones) 1 "ones  + nth")

    (check-equal? (car naturals) 1 "naturals")
    (check-equal? (car ((cdr ((cdr ((cdr naturals))))))) 4 "naturals")
    (check-equal? (nth 1 naturals) 1 "naturals + nth")
    (check-equal? (first 10 naturals) '(1 2 3 4 5 6 7 8 9 10) "fib + first")
    (check-equal? (nth 1000 naturals) 1000 "naturals + nth")

    (check-equal? (car fibs) 1 "fib")
    (check-equal? (car ((cdr ((cdr fibs))))) 2 "fib")
    (check-equal? (first 10 fibs) '(1 1 2 3 5 8 13 21 34 55) "fib + first")
    (check-equal? (nth 50 fibs) 12586269025 "fib + nth")
    (check-equal? (nth 1000 fibs) 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875  "fib + nth")

    (check-equal? (first 5 (squares fibs)) '(1 1 4 9 25) "squares")
    (check-equal? (first 10 (squares naturals)) '(1 4 9 16 25 36 49 64 81 100) "squares")
    (check-equal? (first 20 (squares twos)) '(4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4) "squares")

    (check-equal? (sml nil) '() "sml")
    (check-equal? (sml null (sml nil)) #t "sml")
    (check-equal? (sml hd (sml 5 :: null)) 5 "squares")
    (check-equal? (sml tl (sml 5 :: (sml 4 :: (sml nil)))) '(4) "squares")
))

(run-tests file-tests)