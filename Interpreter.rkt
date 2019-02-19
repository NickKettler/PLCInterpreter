#lang racket

; load simpleParser
(require "simpleParser.rkt")

;returns the value of the code in the filename
(define interpret
  (lambda (filename)
    (format-result (M_value (parser filename) (default-state)))))

;format result to show true and false atoms
(define format-result
  (lambda (value)
    (cond
      ((eq? value #t) 'true)
      ((eq? value #f) 'false)
      (else           value))))

;returns the value of the expression
(define M_value
  (lambda (expression state)
    (cond      
      ((number? expression)                                          expression)
      ((eq? (operator expression) '+)                                (add-statement expression state))
      ((and (eq? (operator expression) '-) (null? (cddr expression)) (unary-statement expression state)))
      ((eq? (operator expression) '-)                                (subtract-statement expression state))
      ((eq? (operator expression) '*)                                (multiply-statement expression state))
      ((eq? (operator expression) '/)                                (divide-statement expression state))
      ((eq? (operator expression) '%)                                (modulus-statement expression state))
      ((eq? (operator expression) '==)                               (equals-statement expression state))
      ((eq? (operator expression) '!=)                               (not-equals-statement expression state))
      ((eq? (operator expression) '<)                                (less-than expression state))
      ((eq? (operator expression) '>)                                (greater-than expression state))
      ((eq? (operator expression) '<=)                               (less-or-equal expression state))
      ((eq? (operator expression) '>=)                               (greater-or-equal expression state))
      ((eq? (operator expression) '&&)                               (and-statement expression state))
      ((eq? (operator expression) '||)                               (or-statement expression state))
      ((eq? (operator expression) '!)                                (not-statement expression state))
      ((eq? (operator expression) 'true)                             #t)
      ((eq? (operator expression) 'false)                            #f)
      ((eq? (operator expression) 'if)                               (if-statement expression state))
      ((eq? (operator expression) 'while)                            (while-statement expression state))
      ((eq? (operator expression) '=)                                (assign-statement expression state))
      ((eq? (operator expression) 'var)                              (add-variable expression state))
      ((eq? (operator expression) 'return)                           (return expression state))
      ((and (list? (operator expression)) (null? (cdr expression)))  (M_value (car expression) state))
      ((list? (operator expression))                                 (M_value (cdr expression) (M_state (car expression) state)))
      (else                                                          (retrieve-value expression state)))))

;state
(define M_state
  (lambda (expression state)
    (cond
      ((eq? (operator expression) 'var)   (add-variable expression state))
      ((eq? (operator expression) '=)     (assign-statement expression state))
      ((eq? (operator expression) 'if)    (if-statement expression state))
      ((eq? (operator expression) 'while) (while-statement expression state))
      (else                               state))))

;default state
(define default-state
  (lambda ()
    '(()())))

;add variable to state
(define add-variable
  (lambda (expression state)
    (if (null? (cddr expression))
        (list (append (state-names state) (list (term1 expression))) (append (state-values state) '(?)))
        (list (append (state-names state) (list (term1 expression))) (append (state-values state) (list (M_value (term2 expression) state)))))))

;find a variable's value
(define retrieve-value
  (lambda (expression state)
    (cond
      ((null? (state-names state)) (error "undeclared variable"))
      ((and (eq? (car (state-names state)) expression) (not (eq? (car (state-values state)) '?))) (car (state-values state)))
      ((and (eq? (car (state-names state)) expression) (eq? (car (state-values state)) '?))       (error "Undeclared variable"))
      (else (retrieve-value expression (list (cdr (state-names state))                            (cdr (state-values state))))))))

;assignment
(define assign-statement
  (lambda (expression state)
    (if(number? (term2 expression))
       (assign-statement-cps expression state (lambda(v) v))
       (assign-statement-cps (list (operator expression) (term1 expression) (M_value (term2 expression) state)) state (lambda(v) v)))))


;assignment
(define assign-statement-cps
  (lambda (expression state return)
    (cond
      [(null? (state-names state))                        (error "Undeclared variable")]
      [(eq? (car (state-names state)) (term1 expression)) (return (list (state-names state) (cons (term2 expression) (cdr (state-values state)))))]
      [else                                               (assign-statement-cps expression (list (cdr (state-names state)) (cdr (state-values state)))
                                                            (lambda(v) (return (list (cons (car (state-names state)) (state-names v))
                                                            (cons (car (state-values state)) (state-values v))))))])))
;state names                                    
(define state-names
  (lambda (state)
    (car state)))

;state values
(define state-values
  (lambda (state)
    (cadr state)))

;return
(define return
  (lambda (expression state)
    (cond
      ((list? (term1 expression))   (M_value (term1 expression) state))
      ((number? (term1 expression)) (M_value (term1 expression) state))
      (else                         (retrieve-value (term1 expression) state))))) 

;if statement
(define if-statement
  (lambda (expression state)
    (if (M_value (conditional expression) state)
        (M_value (then-statement expression) state)
        (if (null? (cdddr expression))
            (M_value (optional-else-statement expression) state)
            state))))

;the condition of the if-statement or while-statement
(define conditional
  (lambda (condition)
    (cadr condition)))

;the then statement of an if-statement
(define then-statement
  (lambda (then)
    (caddr then)))

;the else statement of an if-statement
(define optional-else-statement
  (lambda (else)
    (cadddr else)))

;while statement
(define while-statement
  (lambda (expression state)
    (if (M_value (conditional expression) state)
        (while-statement expression (M_state (body-statement expression) state))
        state)))

;while body statement
(define body-statement
  (lambda (expression)
    (caddr expression)))

;addition
(define add-statement
  (lambda (expression state)
    (+ (M_value (term1 expression) state) (M_value (term2 expression) state))))

;subtraction
(define subtract-statement
  (lambda (expression state)
    (- (M_value (term1 expression) state) (M_value (term2 expression) state))))

;multiplication
(define multiply-statement
  (lambda (expression state)
    (* (M_value (term1 expression) state) (M_value (term2 expression) state))))

;division
(define divide-statement
  (lambda (expression state)
    (quotient (M_value (term1 expression) state) (M_value (term2 expression) state))))

;modulus 
(define modulus-statement
  (lambda (expression state)
    (remainder (M_value (term1 expression) state) (M_value (term2 expression) state))))

;unary 
(define unary-statement
  (lambda (expression state)
    (* -1 (M_value (term1 expression) state))))

;operator
(define operator
  (lambda (expression)
    (if (list? expression)
        (car expression)
        expression)))

;the first term after the operator
(define term1
  (lambda (expression)
    (cadr expression)))

;the second term after the operator
(define term2
  (lambda (expression)
    (caddr expression)))

;equals
(define equals-statement
  (lambda (expression state)
    (eq? (M_value (term1 expression) state) (M_value (term2 expression) state))))

;notequals
(define not-equals-statement
  (lambda (expression state)
    (not (equals-statement expression state))))

;less than
(define less-than
  (lambda (expression state)
    (< (M_value (term1 expression) state) (M_value (term2 expression) state))))

;greater than
(define greater-than
  (lambda (expression state)
    (> (M_value (term1 expression) state) (M_value (term2 expression) state))))

;less than or equal to
(define less-or-equal
  (lambda (expression state)
    (or (equals-statement expression state) (less-than expression state))))

;greater than or equal to
(define greater-or-equal
  (lambda (expression state)
    (or (equals-statement expression state) (greater-than expression state))))

;and
(define and-statement
  (lambda (expression state)
    (and (M_value (term1 expression) state) (M_value (term2 expression) state))))

;or
(define or-statement
  (lambda (expression state)
    (or (M_value (term1 expression) state) (M_value (term2 expression) state))))

;not
(define not-statement
  (lambda (expression state)
    (not (M_value (term1 expression) state))))