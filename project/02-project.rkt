#lang racket

(provide (all-defined-out))

(struct int (n) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct empty () #:transparent)
(struct exception (exn) #:transparent)

(struct ~ (input) #:transparent)
(struct add (input1 input2) #:transparent)
(struct mul (input1 input2) #:transparent)
(struct .. (e1 e2) #:transparent)

(struct ?leq (input1 input2) #:transparent)
(struct ?int (input) #:transparent)
(struct if-then-else (condition input1 input2) #:transparent)

(define (fri expression environment)
    (cond
        [(int? expression) expression]
        [(true? expression) (true)]
        [(false? expression) (false)]
        [(exception? expression) 
            (if (string? (exception-exn expression))
                expression
                (display "Exception must be a string"))
        ]
        [(empty? expression) (empty)]
        [(..? expression) 
            (let (
                [input1 (fri (..-e1 expression) environment)]
                [input2 (fri (..-e2 expression) environment)])
            (list input1 input2)
        )]
        [(add? expression) 
            (let (
                [val1 (fri (add-input1 expression) environment)]
                [val2 (fri (add-input2 expression) environment)])
            (cond
                [(and (int? val1) (int? val2))
                    (int (+ (int-n val1) (int-n val2)))]
                [(or (true? val1) (true? val2))
                    (true)]
                [(and (false? val1) (false? val2))
                    (false)]
                [#t (error "add: wrong argument type")])
        )]
        [(mul? expression) 
            (let (
                [val1 (fri (mul-input1 expression) environment)]
                [val2 (fri (mul-input2 expression) environment)])
            (cond
                [(and (int? val1) (int? val2))
                    (int (* (int-n val1) (int-n val2)))]
                [(and (true? val1) (true? val2))
                    (true)]
                [(or (false? val1) (false? val2))
                    (false)]
                [#t (error "Error, value not an int or bool.")])
        )]
        [(?leq? expression) 
            (let (
                [val1 (fri (?leq-input1 expression) environment)]
                [val2 (fri (?leq-input2 expression) environment)])
            (cond
                [(and (int? val1) (int? val2))
                    (if (<= (int-n val1) (int-n val2))
                        (true)
                        (false))]
                [(and (true? val1) (false? val2))
                    (false)]
                [(or (false? val1) (true? val2))
                    (true)]
                [#t (error "Error, value not an int or bool.")])
        )]
        [(~? expression) 
            (let ([val_input (fri (~-input expression) environment)])
            (cond 
                [(int? val_input) (int (- (int-n val_input)))]
                [(true? val_input) (false)]
                [(false? val_input) (true)]
                [#t (error "Error, can't negate this type of thing.")])
        )]
        [(?int? expression) 
            (let ([expression_input (fri (?int-input expression) environment)])
            (if (int? expression_input)
                (true)
                (false))
        )]
        [(if-then-else? expression) 
            (let ([condition_input (fri (if-then-else-condition expression) environment)])
            (cond 
                [(true? condition_input) (fri (if-then-else-input1 expression) environment)]
                [(false? condition_input) (fri (if-then-else-input2 expression) environment)]
                [#t (error "Error, can't evaluate condition.")])
        )]
        [#t (error "Error, wrong syntax.")]
    ))
