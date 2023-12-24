#lang racket

(provide false true int .. empty exception
         trigger triggered handle
         if-then-else
         ?int ?bool ?.. ?seq ?empty ?exception
         add mul ?leq ?= head tail ~ ?all ?any
         vars valof fun proc closure call
         greater rev binary filtering folding mapping
         fri)

; Data types
(struct true () #:transparent)
(struct false () #:transparent)
(struct int (n) #:transparent)
(struct .. (e1 e2) #:transparent)
(struct empty () #:transparent)
(struct exception (exn) #:transparent)

; Flow Control
(struct trigger (e) #:transparent)
(struct triggered (e) #:transparent)
(struct handle (e1 e2 e3) #:transparent)
(struct if-then-else (condition e1 e2) #:transparent)
(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct ?int (e) #:transparent)
(struct ?bool (e) #:transparent)
(struct ?.. (e) #:transparent)
(struct ?seq (e) #:transparent)
(struct ?empty (e) #:transparent)
(struct ?exception (e) #:transparent)
(struct ?leq (e1 e2) #:transparent)
(struct ?= (e1 e2) #:transparent)
(struct head (e) #:transparent)
(struct tail (e) #:transparent)
(struct ~ (e) #:transparent)
(struct ?all (e) #:transparent)
(struct ?any (e) #:transparent)

; Variables
(struct vars (s e1 e2) #:transparent)
(struct valof (s) #:transparent)

; Functions
(struct fun (name farg body) #:transparent)
(struct proc (name body) #:transparent)
(struct closure (env f) #:transparent)
(struct call (e args) #:transparent)

(define (fri expression environment)
    (cond
        ; Data types
        [(true? expression) expression]
        [(false? expression) expression]
        [(int? expression) expression]
        [(empty? expression) expression]
        [(exception? expression) 
            (if (string? (exception-exn expression))
                expression
                (triggered (exception "expression: must be a string")))
        ]
        [(..? expression) 
            (let (
                [input1 (fri (..-e1 expression) environment)]
                [input2 (fri (..-e2 expression) environment)])
            (cond
                [(triggered? input1) input1]
                [(triggered? input2) input2]
                [#t (.. input1 input2)])
        )]

        ; Flow Control
        [(trigger? expression) 
            (let ([expression_input (fri (trigger-e expression) environment)])
            (cond
                [(triggered? expression_input) expression_input]
                [(exception? expression_input) (triggered expression_input)]
                [#t (triggered (exception "trigger: wrong argument type"))])
        )]
        [(handle? expression) 
            (let (
                [input1 (fri (handle-e1 expression) environment)]
                [input2 (fri (handle-e2 expression) environment)]
                [input3 (fri (handle-e3 expression) environment)])
            (if (triggered? input1) 
                input1
                (if (not (exception? input1))
                    (triggered (exception "handle: wrong argument type"))
                    (if (and (triggered? input2) (equal? input1 (triggered-e input2)))
                        input3
                        input2)))
        )]
        [(if-then-else? expression) 
            (let ([condition_input (fri (if-then-else-condition expression) environment)])
            (cond 
                [(triggered? condition_input) condition_input]
                [(true? condition_input) (fri (if-then-else-e1 expression) environment)]
                [(false? condition_input) (fri (if-then-else-e2 expression) environment)]
                [(int? condition_input) (fri (if-then-else-e1 expression) environment)]
                [#t (triggered (exception "if-then-else: wrong argument type"))])
        )]
        [(?int? expression) 
            (let ([expression_input (fri (?int-e expression) environment)])
            (cond
                [(triggered? expression_input) expression_input]
                [(int? expression_input) (true)]
                [#t (false)])
        )]
        [(?bool? expression) 
            (let ([expression_input (fri (?bool-e expression) environment)])
            (cond
                [(triggered? expression_input) expression_input]
                [(or (true? expression_input) (false? expression_input)) (true)]
                [#t (false)])
        )]
        [(?..? expression) 
            (let ([expression_input (fri (?..-e expression) environment)])
            (cond
                [(triggered? expression_input) expression_input]
                [(..? expression_input) (true)]
                [#t (false)])
        )]
        [(?seq? expression) 
            (let ([expression_input (fri (?seq-e expression) environment)])
            (cond
                [(triggered? expression_input) expression_input]
                [(empty? expression_input) (true)]
                [(not (..? expression_input)) (false)]
                [(if (empty? (..-e2 expression_input)) 
                    (true)
                    (fri (?seq (..-e2 expression_input)) environment))]
                [#t (false)])
        )]
        [(?empty? expression) 
            (let ([expression_input (fri (?empty-e expression) environment)])
            (cond
                [(triggered? expression_input) expression_input]
                [(empty? expression_input) (true)]
                [#t (false)])
        )]
        [(?exception? expression) 
            (let ([expression_input (fri (trigger-e expression) environment)])
            (cond
                [(triggered? expression_input) expression_input]
                [(exception? expression_input) (true)]
                [#t (false)])
        )]
        [(add? expression) 
            (let (
                [val1 (fri (add-e1 expression) environment)]
                [val2 (fri (add-e2 expression) environment)])
            (cond
                [(triggered? val1) val1]
                [(triggered? val2) val2]
                [(and (int? val1) (int? val2))
                    (int (+ (int-n val1) (int-n val2)))]
                [(or (int? val1) (int? val2))
                    (triggered (exception "add: wrong argument type"))]
                [(empty? val1) val2]
                [(empty? val2) val1]
                [(and (..? val1) (..? val2))
                    (let (
                        [head_of_1 (..-e1 val1)]
                        [tail_of_1 (..-e2 val1)])
                    (cond
                        [(empty? tail_of_1) (.. head_of_1 val2)]
                        [(..? tail_of_1) (.. head_of_1 (fri (add tail_of_1 val2) environment))]
                        [#t (triggered (exception "add: wrong argument type"))]
                    )
                )]
                [(or (..? val1) (..? val2))
                    (triggered (exception "add: wrong argument type"))]
                [(or (true? val1) (true? val2))
                    (true)]
                [(and (false? val1) (false? val2))
                    (false)]
                [#t (triggered (exception "add: wrong argument type"))])
        )]
        [(mul? expression) 
            (let (
                [val1 (fri (mul-e1 expression) environment)]
                [val2 (fri (mul-e2 expression) environment)])
            (cond
                [(triggered? val1) val1]
                [(triggered? val2) val2]
                [(and (int? val1) (int? val2))
                    (int (* (int-n val1) (int-n val2)))]
                [(or (int? val1) (int? val2))
                    (triggered (exception "mul: wrong argument type"))]
                [(and (true? val1) (true? val2))
                    (true)]
                [(or (false? val1) (false? val2))
                    (false)]
                [#t (triggered (exception "mul: wrong argument type"))])
        )]
        [(?leq? expression) 
            (let (
                [val1 (fri (?leq-e1 expression) environment)]
                [val2 (fri (?leq-e2 expression) environment)])
            (cond
                [(triggered? val1) val1]
                [(triggered? val2) val2]
                [(and (int? val1) (int? val2))
                    (if (<= (int-n val1) (int-n val2))
                        (true)
                        (false))]
                [(or (int? val1) (int? val2))
                    (triggered (exception "?leq: wrong argument type"))]
                [(empty? val1) (true)]
                [(empty? val2) (false)]
                [(and (..? val1) (..? val2))
                    (let (
                        [tail_of_1 (..-e2 val1)]
                        [tail_of_2 (..-e2 val2)])
                    (fri (?leq tail_of_1 tail_of_2) environment)
                )]
                [(or (..? val1) (..? val2))
                    (triggered (exception "add: wrong argument type"))]
                [(and (true? val1) (false? val2))
                    (false)]
                [(or (false? val1) (true? val2))
                    (true)]
                [#t (triggered (exception "?leq: wrong argument type"))])
        )]
        [(?=? expression) 
            (let (
                [expression1 (fri (?=-e1 expression) environment)]
                [expression2 (fri (?=-e2 expression) environment)])
            (cond
                [(triggered? expression1) expression1]
                [(triggered? expression2) expression2]
                [(equal? expression1 expression2) (true)]
                [#t (false)])
        )]
        [(head? expression) 
            (let ([eval_list (fri (head-e expression) environment)])
            (cond
                [(triggered? eval_list) eval_list]
                [(empty? eval_list)
                    (triggered (exception "head: empty sequence"))]
                [(..? eval_list) (..-e1 eval_list)]
                [#t (triggered (exception "head: wrong argument type"))])
        )]
        [(tail? expression) 
            (let ([eval_list (fri (tail-e expression) environment)])
            (cond
                [(triggered? eval_list) eval_list]
                [(empty? eval_list)
                    (triggered (exception "tail: empty sequence"))]
                [(..? eval_list) (..-e2 eval_list)]
                [#t (triggered (exception "tail: wrong argument type"))])
        )]
        [(~? expression) 
            (let ([expression_input (fri (~-e expression) environment)])
            (cond 
                [(triggered? expression_input) expression_input]
                [(int? expression_input) (int (- (int-n expression_input)))]
                [(true? expression_input) (false)]
                [(false? expression_input) (true)]
                [#t (triggered (exception "~: wrong argument type"))])
        )]
        [(?all? expression) 
            (letrec (
                [expression_input (fri (?all-e expression) environment)]
                [seq_flag (fri (?seq expression_input) environment)])
            (cond
                [(triggered? expression_input) expression_input]
                [(false? seq_flag) (triggered (exception "?all: wrong argument type"))]
                [(empty? expression_input) (true)]
                [(..? expression_input)
                    (let (
                        [head_of_1 (..-e1 expression_input)]
                        [tail_of_1 (..-e2 expression_input)])
                    (if (false? head_of_1)
                        (false)
                        (fri (?all tail_of_1) environment))
                )]
                [#t (triggered (exception "?all: wrong argument type"))])
        )]
        [(?any? expression) 
            (letrec (
                [expression_input (fri (?any-e expression) environment)]
                [seq_flag (fri (?seq expression_input) environment)])
            (cond
                [(triggered? expression_input) expression_input]
                [(false? seq_flag) (triggered (exception "?any: wrong argument type"))]
                [(empty? expression_input) (false)]
                [(..? expression_input)
                    (let (
                        [head_of_1 (..-e1 expression_input)]
                        [tail_of_1 (..-e2 expression_input)])
                    (if (true? head_of_1)
                        (true)
                        (fri (?all tail_of_1) environment))
                )]
                [#t (triggered (exception "?any: wrong argument type"))])
        )]

        ; Variables
        [(vars? expression) expression] ;TODO

        [(valof? expression) expression] ;TODO

        ; Functions
        [(fun? expression) expression] ;TODO

        [(proc? expression) expression] ;TODO

        [(closure? expression) expression] ;TODO

        [(call? expression) expression] ;TODO

        [#t (triggered (exception "fri: wrong syntax."))]
    ))

; Macros
(define (greater e1 e2)
    (fri (int 1) null))

(define (rev e)
    (fri (int 1) null))

(define (binary e)
    (fri (int 1) null))

(define (mapping f seq)
    (fri (int 1) null))

(define (filtering f seq)
    (fri (int 1) null))

(define (folding f init seq)
    (fri (int 1) null))

