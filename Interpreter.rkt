#lang racket
;Nicholas Kettler
;Martin Peters
;Group 34
;EECS 345


;load functionParser
(require "classParser.rkt")

;returns the value of the code in the filename
(define interpret
  (lambda (filename)
    (format-result
     (call/cc
      (lambda (return)
       (run (M_state (parser filename) (default-state) return 'not 'not 'not) return))))))

;format result to show true and false atoms
(define format-result  ;;This method is bypassed by call/cc
  (lambda (value)
    (cond
      ((eq? value #t) 'true)
      ((eq? value #f) 'false)
      (else           value))))

;runs the main method of a given program
(define run
  (lambda (state return)
    (function-call 'main '() state return 'not 'not 'not)))

;returns the value of the expression
(define M_value
  (lambda (expression state return)
    (cond      
      ((number? (operator expression))                               (operator expression))
      ((eq? (operator expression) 'funcall)                          (function-call (function-name expression)
                                                                                    (function-inputs expression)
                                                                                    state
                                                                                    return
                                                                                    'not
                                                                                    'not
                                                                                    'not))
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
      ((eq? (operator expression) #t)                                #t)
      ((eq? (operator expression) #f)                                #f)
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
      ((eq? (operator expression) 'class)    (add-class expression state))
      ((eq? (operator expression) 'dot)      (dot-function expression state return break continue throw))
      ((eq? (operator expression) 'var)      (add-variable expression state return))
      ((eq? (operator expression) '=)        (assign-statement expression state return))
      ((eq? (operator expression) 'if)       (if-statement expression state return break continue throw))
      ((eq? (operator expression) 'while)    (while-statement expression state return break continue throw))
      ((eq? (operator expression) 'begin)    (pop (M_state (cdr expression) (enter-block state) return break continue throw)))
      ((eq? (operator expression) 'return)   (return (M_state (cdr expression) state return break continue throw)))
      ((eq? (operator expression) 'break)    (if (eq? break 'not)
                                               (error "break not in while")
                                               (break (pop state))));;should return error if break == 'not
      ((eq? (operator expression) 'continue) (continue (pop state)))
      ((eq? (operator expression) 'try)      (interpret-try expression state return break continue throw))
      ((eq? (operator expression) 'throw)    (throw (cadr expression) state))
      ((eq? (operator expression) 'function) (add-function (function-name expression)
                                                           (closure-format expression state return)
                                                           state
                                                           return))
      ((list? (operator expression))         (M_state (cdr expression)
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

;add function to environment
(define add-function
  (lambda (name closure state return)
    (modify-top (list (append (top-names state)
                              (list name))
                      (append (top-values state)
                              (list closure))) state)))

;closure formatting
(define closure-format
  (lambda (expression state return)
    (list (param-list expression) (function-body expression) state)))

;function-call
(define function-call
 (lambda (name params state return break continue throw)
    (call/cc
     (lambda (call-return)
       (let ([new-state (add-parameters (car (retrieve-function name state return)) params (enter-block state) state)])
         (M_state (function-code (retrieve-function name state return)) new-state call-return break continue throw))))))
                
;helper functions for add-function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;name
(define function-name
  (lambda (expression)
    (cadr expression)))

;parameters
(define param-list
  (lambda (expression)
    (caddr expression)))

;function body
(define function-body
  (lambda (expression)
    (cadddr expression)))

;executed function code
(define function-code
  (lambda expression
    (cadar expression)))

;Returns inputs for a called function
(define function-inputs
  (lambda expression
    (if (null? (cddar expression))
        '()
        (cddar expression))))

;add variables defined by function params
(define add-parameters
  (lambda (names values function-state original-state)
    (cond
      ((null? names)                              function-state)
      ((not (eq? (length names) (length values))) (error "incorrect number of parameters"))
      (else                                       (add-parameters (cdr names)
                                                                  (cdr values)
                                                                  (add-variable
                                                                   (format-variable-declaration (car names) (value-convert (car values) original-state))
                                                                   function-state
                                                                   'not)
                                                                  original-state)))))

;convert inputs to M_value of inputs
(define value-convert
  (lambda (value state)
    (M_value value state 'not)))

(define add-states
  (lambda (to-add current)
    (if (null? to-add)
      current
      (add-states (cdr to-add) (add-parameters (top-names to-add) (top-values to-add) (enter-block current) current))))) 

;find a variable's value
(define retrieve-value
  (lambda (expression state return)
    (cond
      ((and (null? (top-names state)) (null? (pop state)))                                    (error "undeclared variable"))
      ((null? (top-names state))                                                              (retrieve-value expression (pop state) return))
      ((and (eq? (car (top-names state)) expression) (not (eq? (car (top-values state)) '?))) (car (top-values state)))
      ((and (eq? (car (top-names state)) expression) (eq? (car (top-values state)) '?))       (error "No assigned value"))
      (else (retrieve-value expression (cons (list (cdr (top-names state)) (cdr (top-values state))) (cdr state)) return)))))

;find a functions's instructions
(define retrieve-function
  (lambda (expression state return)
    (cond
      ((and (null? (top-names state)) (null? (pop state)))  (error "undeclared function"))
      ((null? (top-names state))                            (retrieve-function expression (pop state) return))
      ((eq? (car (top-names state)) expression)              (car (top-values state)))
      (else                                                 (retrieve-function
                                                             expression
                                                             (cons (list (cdr (top-names state))
                                                                         (cdr (top-values state)))
                                                                   (cdr state))
                                                             return)))))

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

; Interpret a try-catch-finally block

;Known issue - Does not give correct error if throw in catch block

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
                                                 (add-variable (format-variable-declaration (caadr catch-statement) ex)
                                                               (push (empty-layer) env)
                                                               return)
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
              (new-throw (create-throw-catch-continuation (catch statement) environment return break continue throw jump finally-block)))
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
      (else (cons 'begin (cadr finally-statement))))))

;try block
(define try
  (lambda (expression)
    (cadr expression)))

;catch block
(define catch
  (lambda (expression)
    (caddr expression)))

;error
(define error-block
  (lambda (expression)
    (cadr (catch expression))))

;finally block
(define finally
  (lambda (expression)
    (cadddr expression)))

;format throw variable declaration
(define format-variable-declaration
  (lambda (name value)
    (list '= name value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THIS SECTION HANDLES CLASSES      ;;

(define add-class
  (lambda (expression state)
    (let* ((newnames (append (class-names state) (class-name expression)))
           (newclosures (append (class-closures state) (list (superclass expression) (fields expression))))
           (newclasslevel (list newnames newclosures)))
      (cons (newclasslevel (cdr state))))))

(define class-names
  (lambda (state)
    (caar state)))

(define class-closures
  (lambda (state)
    (cadar state)))
                      
(define class-name
  (lambda (expression)
  (cadr expression)))

(define superclass
  (lambda (expression)
    (caddr expression)))

(define fields
  (lambda (expression)
  (cadddr expression)))

(define dot-function
  (lambda (expression state return break continue throw)
    (get-function (caddr expression) (retrieve-instance (class-name expression) (cdr state)))))

(define f-name
  (lambda (expression)
    (caddr expression)))

(define get-function
  (lambda (name closure)
    ((cond
       ((equals (car (func-names closure)) names) (car (func-defs closure)))
       (else                                      (get-function name
                                                                (list
                                                                 (cdr (func-names closure))
                                                                 (cdr (func-defs closure)))))))))

(define func-names
  (lambda (closure)
    (car closure)))

(define func-defs
  (lambda (closure)
    (cadr closure)))
     
(define retrieve-instance
  (lambda (name state)
    ((cond
      ((and (null? (top-names state)) (null? (pop state)))  (error "undeclared instance"))
      ((null? (top-names state))                            (retrieve-instance name (pop state) return))
      ((eq? (car (top-names state)) name)              (car (top-values state)))
      (else                                                 (retrieve-instance
                                                             name
                                                             (cons (list (cdr (top-names state))
                                                                         (cdr (top-values state)))
                                                                   (cdr state))
                                                             return)))))