#lang racket

(provide (all-defined-out))

(struct int (input) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct add (val1 val2) #:transparent)
(struct mul (val1 val2) #:transparent)
(struct ~ (input) #:transparent)
(struct ?int (input) #:transparent)

(define (fri expression)
    (cond
        [(int? expression) expression]
        [(true? expression) (true)]
        [(false? expression) (false)]
        [(add? expression) 
            (let (
                [val1 (fri (add-val1 expression))]
                [val2 (fri (add-val2 expression))])
            (cond
                [(and (int? val1) (int? val2))
                    (int (+ (int-input val1) (int-input val2)))]
                [(or (true? val1) (true? val2))
                    (true)]
                [(and (false? val1) (false? val2))
                    (false)]
                [#t (error "Value not an int or bool!")])
        )]
        [(mul? expression) 
            (let (
                [val1 (fri (mul-val1 expression))]
                [val2 (fri (mul-val2 expression))])
            (cond
                [(and (int? val1) (int? val2))
                    (int (* (int-input val1) (int-input val2)))]
                [(and (true? val1) (true? val2))
                    (true)]
                [(or (false? val1) (false? val2))
                    (false)]
                [#t (error "Value not an int or bool!")])
        )]
        [(~? expression) 
            (let ([val_input (fri (~-input expression))])
            (cond 
                [(int? val_input) (int (- (int-input val_input)))]
                [(true? val_input) (false)]
                [(false? val_input) (true)]
                [#t (error "Cant negate this type of thing")])
        )]
        [(?int? expression) 
            (let ([expression_input (fri (?int-input expression))])
            (if (int? expression_input)
                (true)
                (false))
        )]
    ))


(fri (?int (false)))

(struct konst (int) #:transparent)     ; konstanta; argument je število
(struct bool (b) #:transparent)        ; b ima lahko vrednost true or false
(struct negiraj (e) #:transparent)     ; e je lahko izraz
(struct sestej (e1 e2) #:transparent)  ; e1 in e2 sta izraza
(struct ce-potem-sicer (pogoj res nires) #:transparent) ; pogoj, res, nires hranijo izraze

(define (jais e)
  (cond [(konst? e) e]   ; vrnemo izraz v ciljnem jeziku
        [(bool? e) e]
        [(negiraj? e) 
         (let ([v (jais (negiraj-e e))])
           (cond [(konst? v) (konst (- (konst-int v)))]
                 [(bool? v) (bool (not (bool-b v)))]
                 [#t (error "negacija nepričakovanega izraza")]))]
        [(sestej? e) 
         (let ([v1 (jais (sestej-e1 e))]
               [v2 (jais (sestej-e2 e))])
           (if (and (konst? v1) (konst? v2))
               (konst (+ (konst-int v1) (konst-int v2)))
               (error "seštevanec ni številka")))]
        [(ce-potem-sicer? e) 
         (let ([v-test (jais (ce-potem-sicer-pogoj e))])
           (if (bool? v-test)
               (if (bool-b v-test)
                   (jais (ce-potem-sicer-res e))
                   (jais (ce-potem-sicer-nires e)))
               (error "pogoj ni logična vrednost")))]
        [#t (error "sintaksa izraza ni pravilna")]
        ))