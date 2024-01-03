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
                [(false? condition_input) (fri (if-then-else-e2 expression) environment)]
                [#t (fri (if-then-else-e1 expression) environment)])
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
                    ))]
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
                    (fri (?leq tail_of_1 tail_of_2) environment))]
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
                        (fri (?all tail_of_1) environment)))]
                [#t (triggered (exception "?any: wrong argument type"))])
        )]

        ; Variables
        [(vars? expression) 
            (let (
                [names (vars-s expression)]
                [vals (vars-e1 expression)]
                [expression_input (vars-e2 expression)])
            (cond
                [(and (list? names) (list? vals)) 
                    (let ([fried_values (map (λ (value) (fri value environment)) vals)])
                    (if (triggered? fried_values) 
                        fried_values
                        (fri expression_input (append (map (λ (n v) (cons n v)) names fried_values) environment))))]
                [(string? names) 
                    (let ([evaluated_value (fri vals environment)]) 
                        (if (triggered? evaluated_value)
                            evaluated_value
                            (fri expression_input (append (list (cons names evaluated_value)) environment))))]
                [#t (triggered (exception "vars: wrong argument type"))])
        )]
        [(valof? expression) 
            (letrec (
                [name (valof-s expression)]
                [found (assoc name environment)])
            (cond
                [found (cdr found)]
                [#t (triggered (exception "valof: undefined variable"))])
        )]

        ; Functions
        [(fun? expression) (closure environment expression)]
        [(proc? expression) expression]
        [(call? expression) 
            (letrec (
                [e (fri (call-e expression) environment)]
                [args (call-args expression)]
                [fried_args (map (λ (v) (fri v environment)) args)])
            (cond
                [(triggered? e) e]
                [(proc? e)
                    (let (
                        [name (proc-name e)]
                        [body (proc-body e)])
                    (if (null? fried_args)
                        (fri body (append (list (cons name null)) environment))
                        (triggered (exception "call: arity mismatch")))
                )]
                [(closure? e) 
                    (letrec (
                        [env (closure-env e)]
                        [f (closure-f e)]
                        [name (fun-name f)]
                        [farg (fun-farg f)]
                        [body (fun-body f)])
                    (if (equal? (length fried_args) (length farg))
                        (fri body (append (map (λ (n v) (cons n v)) farg fried_args) (list (cons name e)) env))
                        (triggered (exception "call: arity mismatch")))
                )]
                [#t (triggered (exception "call: wrong argument type"))])
        )]

        ; Default case
        [#t (triggered (exception "fri: wrong syntax"))]
    ))

; Macros
(define (greater e1 e2)
    (vars 
        (list "ee1" "ee2")
        (list (fri e1 null) (fri e2 null))
    (if-then-else (?= (valof "ee1") (valof "ee2"))
        (false)
        (?leq (valof "ee2") (valof "ee1")))
))

(define (rev e)
    (call 
        (fun "reverse" (list "e" "acc")
            (vars 
                (list "head" "tail")
                (list (head (valof "e")) (tail (valof "e")))
                (if-then-else (?empty (valof "tail"))
                    (.. (valof "head") (valof "acc"))
                    (call (valof "reverse") (list (valof "tail") (.. (valof "head") (valof "acc")))))))
        (list e (empty)))
)

(define (binary e1)
    (triggered (exception "binary: not implemented"))
)

(define (mapping f seq)
    (call 
        (fun "map_inner" (list "f" "seq")
            (vars 
                (list "head" "tail")
                (list (head (valof "seq")) (tail (valof "seq")))
                (if-then-else (?empty (valof "tail"))
                    (call (valof "f") (list (valof "head")))
                    (.. (call (valof "f") (list (valof "head"))) (call (valof "map_inner") (list (valof "f") (valof "tail")))))))
        (list f seq))
)

(define (filtering f seq)
    (call 
        (fun "filter_inner" (list "f" "seq")
            (vars 
                (list "head" "tail")
                (list (head (valof "seq")) (tail (valof "seq")))
                (if-then-else (?empty (valof "tail"))
                    (if-then-else (call (valof "f") (list (valof "head")))
                        (.. (valof "head") (empty))
                        (empty)
                    (if-then-else (call (valof "f") (list (valof "head"))))
                        (.. (valof "head") (call (valof "filter_inner") (list (valof "f") (valof "tail"))))
                        (call (valof "filter_inner") (list (valof "f") (valof "tail")))))))
        (list f seq))
)

(define (folding f init seq)
        (call 
        (fun "fold_inner" (list "f" "init" "seq")
            (vars 
                (list "head" "tail")
                (list (head (valof "seq")) (tail (valof "seq")))
                (if-then-else (?empty (valof "tail"))
                    (call (valof "f") (list (valof "init")))
                    (call (valof "f") (list (call (valof "fold_inner") (list (valof "f") (valof "tail"))))))))
        (list f init seq))
)


;(fri (rev (.. (int 0) (.. (int 1) (.. (int 2) (.. (int 3) (empty)))))) null)

;(fri (mapping (fun "" null (valof "x")) (.. (int 0) (.. (int 1) (.. (int 2) (.. (int 3) (empty)))))) null)