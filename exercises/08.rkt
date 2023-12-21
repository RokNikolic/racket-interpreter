#lang racket

(provide (all-defined-out))

(struct int (input) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct add (input1 input2) #:transparent)
(struct mul (input1 input2) #:transparent)
(struct ?leq (input1 input2) #:transparent)
(struct ~ (input) #:transparent)
(struct ?int (input) #:transparent)
(struct if-then-else (condition input1 input2) #:transparent)

(define (fri expression)
    (cond
        [(int? expression) expression]
        [(true? expression) (true)]
        [(false? expression) (false)]
        [(add? expression) 
            (let (
                [val1 (fri (add-input1 expression))]
                [val2 (fri (add-input2 expression))])
            (cond
                [(and (int? val1) (int? val2))
                    (int (+ (int-input val1) (int-input val2)))]
                [(or (true? val1) (true? val2))
                    (true)]
                [(and (false? val1) (false? val2))
                    (false)]
                [#t (error "Error, value not an int or bool.")])
        )]
        [(mul? expression) 
            (let (
                [val1 (fri (mul-input1 expression))]
                [val2 (fri (mul-input2 expression))])
            (cond
                [(and (int? val1) (int? val2))
                    (int (* (int-input val1) (int-input val2)))]
                [(and (true? val1) (true? val2))
                    (true)]
                [(or (false? val1) (false? val2))
                    (false)]
                [#t (error "Error, value not an int or bool.")])
        )]
        [(?leq? expression) 
            (let (
                [val1 (fri (?leq-input1 expression))]
                [val2 (fri (?leq-input2 expression))])
            (cond
                [(and (int? val1) (int? val2))
                    (if (<= (int-input val1) (int-input val2))
                        (true)
                        (false))]
                [(or (true? val1) (false? val2))
                    (false)]
                [(or (false? val1) (true? val2))
                    (true)]
                [#t (error "Error, value not an int or bool.")])
        )]
        [(~? expression) 
            (let ([val_input (fri (~-input expression))])
            (cond 
                [(int? val_input) (int (- (int-input val_input)))]
                [(true? val_input) (false)]
                [(false? val_input) (true)]
                [#t (error "Error, can't negate this type of thing.")])
        )]
        [(?int? expression) 
            (let ([expression_input (fri (?int-input expression))])
            (if (int? expression_input)
                (true)
                (false))
        )]
        [(if-then-else? expression) 
            (let ([condition_input (fri (if-then-else-condition expression))])
            (cond 
                [(true? condition_input) (fri (if-then-else-input1 expression))]
                [(false? condition_input) (fri (if-then-else-input2 expression))]
                [#t (error "Error, can't evaluate condition.")])
        )]
        [#t (error "Error, wrong syntax.")]
    ))

(define (conditional condition_input input . other)
    (if  (equal? 1 (length other)) 
        (if-then-else condition_input input (car other))
        (if-then-else condition_input input (apply conditional other))))

(define (?geq expresion1 expression2)
    (?leq expression2 expresion1))
