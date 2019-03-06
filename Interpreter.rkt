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
    (call/cc
     (lambda (return)
       (format-result (M_value (parser filename) (default-state) return))))))

;format result to show true and false atoms
(define format-result
  (lambda (value)
    (cond
      ((eq? value #t) 'true)
      ((eq? value #f) 'false)
      (else           value))))

;returns the value of the expression
(define M_value
  (lambda (expression state return)
    (cond      
      ((number? expression)                                          expression)
      ((eq? (operator expression) '+)                                (math-statement + expression state return))
      ((and (eq? (operator expression) '-) (null? (cddr expression)) (unary-statement expression state return)))
      ((eq? (operator expression) '-)                                (math-statement - expression state return))
      ((eq? (operator expression) '*)                                (math-statement * expression state return))
      ((eq? (operator expression) '/)                                (math-statement quotient expression state return))
      ((eq? (operator expression) '%)                                (math-statement remainder expression state return))
      ((eq? (operator expression) '==)                               (comparison-statement eq? expression state return))
      ((eq? (operator expression) '!=)                               (not (comparison-statement eq? expression state return)))
      ((eq? (operator expression) '<)                                (comparison-statement < expression state return))
      ((eq? (operator expression) '>)                                (comparison-statement > expression state return))
      ((eq? (operator expression) '<=)                               (or
                                                                      (comparison-statement eq? expression state return)
                                                                      (comparison-statement < expression state return)))
      ((eq? (operator expression) '>=)                               (or
                                                                      (comparison-statement eq? expression state return)
                                                                      (comparison-statement > expression state return)))
      ((eq? (operator expression) '&&)                               (and-statement expression state return))
      ((eq? (operator expression) '||)                               (or-statement expression state return))
      ((eq? (operator expression) '!)                                (not-statement expression state return))
      ((eq? (operator expression) 'true)                             #t)
      ((eq? (operator expression) 'false)                            #f)
      ((eq? (operator expression) 'if)                               (if-statement expression state return))
      ((eq? (operator expression) 'while)                            (while-statement expression state return))
      ((eq? (operator expression) '=)                                (assign-statement expression state return))
      ((eq? (operator expression) 'var)                              (add-variable expression state return))
      ((eq? (operator expression) 'return)                           (return (M_value (cdr expression) state return)))
      ((and (list? (operator expression)) (null? (cdr expression)))  (M_value (car expression) state return))
      ((list? (operator expression))                                 (M_value
                                                                      (cdr expression)
                                                                      (M_state (car expression) state return)
                                                                      return))
      ((list? expression)                                            (retrieve-value (car expression) state return))
      (else                                                          (retrieve-value expression state return)))))

;state
(define M_state
  (lambda (expression state return)
    (cond
      ((eq? (operator expression) 'var)   (add-variable expression state return))
      ((eq? (operator expression) '=)     (assign-statement expression state return))
      ((eq? (operator expression) 'if)    (if-statement expression state return))
      ((eq? (operator expression) 'while) (while-statement expression state return))
      ((eq? (operator expression) 'begin) (M_value (cdr expression) (enter-block state) return))
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
  (lambda (expression state return)
    (if (null? (cddr expression))
        (modify-top (list (append (top-names state) (list (term1 expression))) (append (top-values state) '(?))) state)
        (modify-top (list (append (top-names state)
                                  (list (term1 expression))) (append (top-values state)
                                                                     (list (M_value (term2 expression) state return)))) state))))

;find a variable's value
(define retrieve-value
  (lambda (expression state return)
    (cond
      ((and (null? (top-names state)) (null? (pop state)))                                    (error "undeclared variable"))
      ((null? (top-names state))                                                              (retrieve-value expression (pop state) return))
      ((and (eq? (car (top-names state)) expression) (not (eq? (car (top-values state)) '?))) (car (top-values state)))
      ((and (eq? (car (top-names state)) expression) (eq? (car (top-values state)) '?))       (error "No assigned value"))
      (else (retrieve-value expression (cons (list (cdr (top-names state)) (cdr (top-values state))) (cdr state)) return)))))

;assignment
(define assign-statement
  (lambda (expression state return)
    (if(number? (term2 expression))
       (assign-statement-cps expression state (lambda(v) v))
       (assign-statement-cps (list (operator expression)
                                   (term1 expression)
                                   (M_value (term2 expression) state return)) state (lambda(v) v)))))


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

;if statement
(define if-statement
  (lambda (expression state return)
    (if (M_value (conditional expression) state return)
        (M_value (then-statement expression) state return)
        (if (not (null? (cdddr expression)))
            (M_value (optional-else-statement expression) state return)
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

;while statement interior
(define while-call
  (lambda (expression state return break)
    (cond
      ((M_value (conditional expression) state return) (while-call expression (M_state (body-statement expression) state return) break))
      (else                                     state))))

;while statement starter
(define while-statement
  (lambda (expression state return)    
    (call/cc
     (lambda (break)
       (while-call expression state return break)))))

;while body statement
(define body-statement
  (lambda (expression)
    (caddr expression)))

;all mathematical statements except unary
(define math-statement
  (lambda (operator expression state return)
    (operator (M_value (term1 expression) state return) (M_value (term2 expression) state return))))

;unary 
(define unary-statement
  (lambda (expression state return)
    (* -1 (M_value (term1 expression) state return))))

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
  (lambda (function expression state return)
    (function (M_value (term1 expression) state return) (M_value (term2 expression) state return))))


;and
(define and-statement
  (lambda (expression state return)
    (and (M_value (term1 expression) state return) (M_value (term2 expression) state return))))

;or
(define or-statement
  (lambda (expression state return)
    (or (M_value (term1 expression) state return) (M_value (term2 expression) state return))))

;not
(define not-statement
  (lambda (expression state return)
    (not (M_value (term1 expression) state return))))