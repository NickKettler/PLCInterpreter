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
    (expression)))

;
(define M_state
  (lambda (expression state)
    (state)))

;assignment
(define assign
  (lambda (expression state)
    (expression)))

;variable declaration
(define declare-variable
  (lambda (expression state)
    (expression)))

;return
(define return
  (lambda (expression state)
    (M_value expression state)))

;if statement
(define if-statement
  (lambda (expression state)
    (expression)))

;while statement
(define while-statement
  (lambda (expression state)
    (expression)))

;addition
(define add
  (lambda (expression state)
    (+ (M_value (term1 expression) state) (M_value (term2 expression) state))))

;subtraction
(define subtract
  (lambda (expression state)
    (- (M_value (term1 expression) state) (M_value (term2 expression) state))))

;multiply
(define multiply
  (lambda (expression state)
    (* (M_value (term1 expression) state) (M_value (term2 expression) state))))

;quotient
(define divide
  (lambda (expression state)
    (quotient (M_value (term1 expression) state) (M_value (term2 expression) state))))

;java modulus 
(define modulus-statement
  (lambda (expression state)
    (remainder (M_value (term1 expression) state) (M_value (term2 expression) state))))

;unary 
(define unary
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
(define equals
  (lambda (expression state)
    (eq? (M_value((term1 expression) state))(M_value((term2 expression) state)))))

;notequals
(define not-equals
  (lambda (expression state)
    (not (equals expression state))))

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
    (or (equals expression state) (less-than expression state))))

;greater than or equal to
(define greater-or-equal
  (lambda (expression state)
    (or (equals expression state) (greater-than expression state))))

; and
(define and
  (lambda (expression state)
    (and (M_value (term1 expression) state) (M_value (term2 expression) state))))

;or
(define or
  (lambda (expression state)
    (or (M_value (term1 expression) state) (M_value (term2 expression) state))))

;not
(define not
  (lambda (expression state)
    (not (M_value (term1 expression) state))))