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
    (format-result
     (call/cc
      (lambda (return)
       (M_state (parser filename) (default-state) return 'not 'not 'not))))))

;format result to show true and false atoms
(define format-result  ;;This method is bypassed by call/cc
  (lambda (value)
    (cond
      ((eq? value #t) 'true)
      ((eq? value #f) 'false)
      (else           value))))

;returns the value of the expression
(define M_value
  (lambda (expression state return)
    (cond      
      ((number? (operator expression))                               (operator expression))
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
      ((eq? (operator expression) 'while)                            (while-statement expression state return)) ;;remove
      ((list? expression)                                            (retrieve-value (car expression) state return))
      (else                                                          (retrieve-value expression state return)))))
      

;state
(define M_state
  (lambda (expression state return break continue throw)
    (cond
      [(null? expression)                  state]
      ((eq? (operator expression) 'var)    (add-variable expression state return))
      ((eq? (operator expression) '=)      (assign-statement expression state return))
      ((eq? (operator expression) 'if)     (if-statement expression state return break continue throw))
      ((eq? (operator expression) 'while)  (while-statement expression state return break continue throw))
      ((eq? (operator expression) 'begin)  (pop (M_state (cdr expression) (enter-block state) return break continue throw)))
      ((eq? (operator expression) 'return) (return (M_state (cdr expression) state return break continue throw)))
      ((eq? (operator expression) 'break)  (if (eq? break 'not)
                                               (error "break not in while")
                                               (break (pop state))));;should return error if break == 'not
      ((eq? (operator expression) 'continue) (continue (pop state)))
      ((eq? (operator expression) 'try)    (interpret-try expression state return break continue throw))
      ((list? (operator expression))       (M_state (cdr expression)
                                                    (M_state (car expression) state return break continue throw)
                                                     return
                                                     break
                                                     continue
                                                     throw))
      (else                                (M_value expression state return)))))
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
                                   (M_value (term2 expression) state return)) state (lambda(v) v)))))   ;;probably remove m_value here and change to state


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
  (lambda (expression state return break continue throw)
    (if (M_state (conditional expression) state return break continue throw)
        (M_state (then-statement expression) state return break continue throw)
        (if (not (null? (cdddr expression)))
            (M_state (optional-else-statement expression) state return break continue throw)
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
  (lambda (expression state return break continue throw)
    (if (M_value (conditional expression) state return)
        (while-call expression
                    (call/cc (lambda (k) (M_state (body-statement expression) state return break k throw)))
                    return
                    break
                    continue
                    throw)
        state)))

;while statement starter
(define while-statement
  (lambda (expression state return break continue throw)    
    (call/cc
     (lambda (k)
       (while-call expression state return k continue throw)))))

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
    (function (M_state (term1 expression) state return 'not 'not 'not) (M_state (term2 expression) state return 'not 'not 'not))))


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

;try block statement
(define try-catch
  (lambda (expression state return break continue throw)
    (cond
      [(null? (finally expression)) state]
      [else (M_state (finally expression) (M_state (try expression)
                                                   (lambda (v s2) (M_state
                                                                                     (finally expression)
                                                                                     s2
                                                                                     return
                                                                                     break
                                                                                     continue
                                                                                     throw)
                                                                    return v)
                                                   (lambda (x) (return (M_value x (M_state (finally expression) state return break continue throw))))
                                                   (lambda (bs) (break M_state (finally expression) bs return break continue throw))
                                                   (lambda (ex s3) (M_state (finally expression) (M_state (catch expression) (add-variable (error-block expression) ex (push s3)) return break continue throw)))
                                                   throw)
                     return break continue throw)])))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (M_state finally-block env return break continue throw)))) 
      ((not (eq? 'catch (car catch-statement))) (error "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (M_state (cdr finally-block)
                                     (pop
                                      (M_state 
                                                 (caddr catch-statement) 
                                                 (add-variable (caadr catch-statement) ex (push env))
                                                 return 
                                                 (lambda (env2) (break (pop env2))) 
                                                 (lambda (env2) (continue (pop env2))) 
                                                 (lambda (v env2) (throw v (pop env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (finally statement)))
              (try-block (make-try-block (try statement)))
              (new-return (lambda (v) (begin (M_state finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (M_state finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (M_state finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (caddr statement) environment return break continue throw jump finally-block)))
         (M_state finally-block
                          (M_state try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (car finally-statement) 'finally)) (error "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

;try block
(define try
  (lambda (expression)
    (cdr expression)))

;catch block
(define catch
  (lambda (expression)
    (cddr expression)))

;error
(define error-block
  (lambda (expression)
    (cadr (catch expression))))

;finally block
(define finally
  (lambda (expression)
    (cdddr expression)))