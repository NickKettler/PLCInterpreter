#lang racket
;Nicholas Kettler
;Martin Peters
;Group 34
;EECS 345


;load simpleParser
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
      ((eq? (operator expression) '==)                               (comparison-statement eq? expression state))
      ((eq? (operator expression) '!=)                               (not (comparison-statement eq? expression state)))
      ((eq? (operator expression) '<)                                (comparison-statement < expression state))
      ((eq? (operator expression) '>)                                (comparison-statement > expression state))
      ((eq? (operator expression) '<=)                               (or
                                                                      (comparison-statement eq? expression state)
                                                                      (comparison-statement < expression state)))
      ((eq? (operator expression) '>=)                               (or
                                                                      (comparison-statement eq? expression state)
                                                                      (comparison-statement > expression state)))
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
      ((eq? (operator expression) 'begin) (M_value (cdr expression) (enter-block state)))
      (else                               state))))

;default state
(define default-state
  (lambda ()
    (list (empty-layer))))

;empty layer
(define empty-layer
  (lambda ()
    '(()())))

;pop layer
(define pop
  (lambda (state)
    (cdr state)))

;push layer
(define push
  (lambda (layer state)
    (cons layer state)))

;modify layer
;Takes a state and a layer and returns the new state that is the result of the top layer being replaced by the input layer
(define modify-top
  (lambda (layer state)
    (push layer (pop state))))

;enter new block
(define enter-block
  (lambda (state)
    (push (empty-layer) state)))

;to-layer takes a list of names and a list of values and makes it a layer
(define to-layer
  (lambda (names values)
    (list names values)))

;add variable to state
(define add-variable
  (lambda (expression state)
    (if (null? (cddr expression))
        (modify-top (list (append (top-names state) (list (term1 expression))) (append (top-values state) '(?))) state)
        (modify-top (list (append (top-names state)
                                  (list (term1 expression))) (append (top-values state)
                                                                     (list (M_value (term2 expression) state)))) state))))

;find a variable's value
(define retrieve-value
  (lambda (expression state)
    (cond
      ((and (null? (top-names state)) (null? (pop state)))                                    (error "undeclared variable"))
      ((null? (top-names state))                                                              (retrieve-value expression (pop state)))
      ((and (eq? (car (top-names state)) expression) (not (eq? (car (top-values state)) '?))) (car (top-values state)))
      ((and (eq? (car (top-names state)) expression) (eq? (car (top-values state)) '?))       (error "No assigned value"))
      (else (retrieve-value expression (cons (list (cdr (top-names state)) (cdr (top-values state))) (cdr state)))))))

;assignment
(define assign-statement
  (lambda (expression state)
    (if(number? (term2 expression))
       (assign-statement-cps expression state (lambda(v) v))
       (assign-statement-cps (list (operator expression)
                                   (term1 expression)
                                   (M_value (term2 expression) state)) state (lambda(v) v)))))


;assignment
(define assign-statement-cps
  (lambda (expression state return)
    (cond
      [(null? state)                                    (error "Undeclared variable")]
      [(null?  (top-names state))                       (assign-statement-cps expression (lower-layers state)
                                                                                         (lambda(v) (return (cons (top-layer state)
                                                                                                                   v))))]
      [(eq? (car (top-names state)) (term1 expression)) (return (modify-top (list (top-names state)
                                                                                  (cons (term2 expression)
                                                                                        (cdr (top-values state)))) state))]
      [else                                               (assign-statement-cps expression (modify-top (list (cdr (top-names state))
                                                                                                             (cdr (top-values state)))state)
                                                            (lambda(v) (return (modify-top (list (cons (car (top-names state)) (top-names v))
                                                            (cons (car (top-values state)) (top-values v))) v))))])))
;state names                                    
(define top-names
  (lambda (state)
    (caar state)))

;top layer
(define top-layer
  (lambda (state)
    (car state)))

;lower layers
(define lower-layers
  (lambda (state)
    (cdr state)))

;state values
(define top-values
  (lambda (state)
    (cadar state)))

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
        (if (not (null? (cdddr expression)))
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

;comparison-statement
(define comparison-statement
  (lambda (function expression state)
    (function (M_value (term1 expression) state) (M_value (term2 expression) state))))


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