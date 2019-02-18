#lang racket

; load simpleParser
(require "simpleParser.rkt")

;returns the value of the code in the filename
(define interpret
  (lambda (filename)
    (M_value (parser filename) '(()()))))

;
(define M_value
  (lambda (expression state)
    (cond      
      ((number? expression)                                      expression)
      ((eq? (operator expression) '+)                            (add-statement expression state))
      ((and (eq? (operator expression) '-)  (null? term2))       (unary-statement expression state))
      ((and (eq? (operator expression) '-)  (not (null? term2))) (subtract-statement expression state))
      ((eq? (operator expression) '*)                            (multiply-statement expression state))
      ((eq? (operator expression) '/)                            (divide-statement expression state))
      ((eq? (operator expression) '%)                            (modulus-statement expression state))
      ((eq? (operator expression) '==)                           (equals-statement expression state))
      ((eq? (operator expression) '!=)                           (not-equals-statement expression state))
      ((eq? (operator expression) '<)                            (less-than expression state))
      ((eq? (operator expression) '>)                            (greater-than expression state))
      ((eq? (operator expression) '<=)                           (less-or-equal expression state))
      ((eq? (operator expression) '>=)                           (greater-or-equal expression state))
      ((eq? (operator expression) '&&)                           (and-statement expression state))
      ((eq? (operator expression) '||)                           (or-statement expression state))
      ((eq? (operator expression) '!)                            (not-statement expression state))
      ((eq? (operator expression) 'if)                           (if-statement expression state))
      ((eq? (operator expression) 'while)                        (while-statement expression state))
      ((eq? (operator expression) '=)                            (assign-statement expression state))
      ((eq? (operator expression) 'var)                          (declare-variable expression state))
      ((eq? (operator expression) 'return)                       (return expression state))
      ((number? (operator expression))                           (operator expression))
      ((list? (operator expression))                             (M_value (operator expression) state))
      (else                                                      (M_state expression state)))))

;state
(define M_state
  (lambda (expression state)
    state))

;assignment
(define assign-statement
  (lambda (expression state)
    (expression)))

;variable declaration
(define declare-variable
  (lambda (expression state)
    (expression)))

;return
(define return
  (lambda (expression state)
    (M_value (term1 expression) state)))

;if statement
(define if-statement
  (lambda (expression state)
    (if (conditional expression)
        (M_value((then-statement expression) state expression))
        (if (not(null? (optional-else-statement expression)))
            (M_value (optional-else-statement expression) state) '()))))

(define conditional
  (lambda (condition)
    (cadr condition)))

(define then-statement
  (lambda (then)
    (caddr then)))

(define optional-else-statement
  (lambda (else)
    (cadddr else)))

;while statement
(define while-statement
  (lambda (expression state)
    (expression)))

;addition
(define add-statement
  (lambda (expression state)
    (+ (M_value (term1 expression) state) (M_value (term2 expression) state))))

;subtraction
(define subtract-statement
  (lambda (expression state)
    (- (M_value (term1 expression) state) (M_value (term2 expression) state))))

;multiply
(define multiply-statement
  (lambda (expression state)
    (* (M_value (term1 expression) state) (M_value (term2 expression) state))))

;divide
(define divide-statement
  (lambda (expression state)
    (quotient (M_value (term1 expression) state) (M_value (term2 expression) state))))

;java modulus 
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
    (car expression)))

;term1
(define term1
  (lambda (expression)
    (cadr expression)))

;term2
(define term2
  (lambda (expression)
    (caddr expression)))

;equals
(define equals-statement
  (lambda (expression state)
    (eq? (M_value((term1 expression) state))(M_value((term2 expression) state)))))

;notequals
(define not-equals-statement
  (lambda (expression state)
    (not (equals-statement expression state))))

;less than
(define less-than
  (lambda (expression state)
    (< (M_value((term1 expression) state))(M_value((term2 expression) state)))))

;greater than
(define greater-than
  (lambda (expression state)
    (> (M_value((term1 expression) state))(M_value((term2 expression) state)))))

;less than or equal to
(define less-or-equal
  (lambda (expression state)
    (or (equals-statement expression state) (less-than expression state))))

;greater than or equal to
(define greater-or-equal
  (lambda (expression state)
    (or (equals-statement expression state) (greater-than expression state))))

; and
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