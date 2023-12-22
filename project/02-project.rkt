#lang racket

(provide (all-defined-out))

(struct true () #:transparent)
(struct false () #:transparent)
(struct int (n) #:transparent)
(struct .. (e1 e2) #:transparent)
(struct empty () #:transparent)
(struct exception (exn) #:transparent)

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
(struct ~ (e) #:transparent)
(struct ?all (e) #:transparent)
(struct ?any (e) #:transparent)

(define (fri expression environment)
    (cond
        ; Data types
        [(int? expression) expression]
        [(true? expression) (true)]
        [(false? expression) (false)]
        [(empty? expression) (empty)]
        [(exception? expression) 
            (if (string? (exception-exn expression))
                expression
                (triggered (exception "expression: must be a string")))
        ]
        [(..? expression) 
            (let (
                [input1 (fri (..-e1 expression) environment)]
                [input2 (fri (..-e2 expression) environment)])
            (.. input1 input2)
        )]

        ; Flow Control
        [(trigger? expression) 
            (let (
                [eval_expression (fri (trigger-e expression) environment)])
            (if (exception? eval_expression) 
                (triggered eval_expression)
                (triggered (exception "trigger: wrong argument type")))
        )]
        [(handle? expression) expression] ;TODO

        [(if-then-else? expression) 
            (let ([condition_input (fri (if-then-else-condition expression) environment)])
            (cond 
                [(triggered? condition_input) condition_input]
                [(true? condition_input) (fri (if-then-else-e1 expression) environment)]
                [(false? condition_input) (fri (if-then-else-e2 expression) environment)]
                [#t (triggered (exception "if-then-else: wrong argument type"))])
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
        [(?int? expression) 
            (let ([expression_input (fri (?int-e expression) environment)])
            (if (int? expression_input)
                (true)
                (false))
        )]
        [(?bool? expression) expression] ;TODO

        [(?..? expression) expression] ;TODO

        [(?seq? expression) expression] ;TODO

        [(?empty? expression) expression] ;TODO

        [(?exception? expression) expression] ;TODO

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
                [(and (true? val1) (false? val2))
                    (false)]
                [(or (false? val1) (true? val2))
                    (true)]
                [#t (triggered (exception "?leq: wrong argument type"))])
        )]
        [(?=? expression) expression] ;TODO

        [(head? expression) expression] ;TODO

        [(~? expression) 
            (let ([val_input (fri (~-e expression) environment)])
            (cond 
                [(triggered? val_input) val_input]
                [(int? val_input) (int (- (int-n val_input)))]
                [(true? val_input) (false)]
                [(false? val_input) (true)]
                [#t (triggered (exception "~: wrong argument type"))])
        )]
        [(?all? expression) expression] ;TODO

        [(?any? expression) expression] ;TODO

        ; Variables
        [#t (triggered (exception "fri: wrong syntax."))]
    ))

(fri (add (add (int 9) (int 9)) (true)) null)