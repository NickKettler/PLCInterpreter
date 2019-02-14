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
    (return (cadr expression))))

;term2
(define term2
  (lambda (expression)
    (return (caddr expression))))

;