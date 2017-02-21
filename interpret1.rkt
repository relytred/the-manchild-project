#|
Jeffrey Noymer & Tyler Mayle
jan88 & tcm45
Project 1
|#

(load "simpleParser.scm")
(load "control.rkt")


; A method to take in an external string file and read it into our interpreter

(define interpret
  (lambda (expr)
    (statement (parser expr) '(()()))
    ))

; Defining our tree parsing for statments
(define type caar)
(define expr1 cadar)
(define expr2 caddar)

; A method that facilitates the action in the parsed document

(define statement
  (lambda (expr state)
    (cond
      ((eq? (type expr) 'return) (value (expr1 expr) state))
      ((eq? (type expr) 'var) (statement (cdr expr) (declareVariable (expr1 expr) state)))
      ((eq? (type expr) '=) (statement (cdr expr) (assignVariable (expr1 expr) (expr2 expr) state)))
      ((eq? (type expr) 'if) (statement (cdr expr) (ifEval expr state)))
      ((eq? (type expr) 'while) (whileEval expr)) )))

; Helper methods to determine which element is an operator or an operand in the statemnt

(define operator car)
(define operand1 cadr)
(define operand2 caddr)

; A method to evaluate all of the boolean operations and update their states

(define boolean
  (lambda (expr state)
    (cond
      ((eq? (operator expr) '&&) (and (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '||) (or (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '==) (eq? (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '!=) (not (eq? (value (operand1 expr) state) (value (operand2 expr) state))))
      ((eq? (operator expr) '<=) (<= (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '>=) (>= (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '>) (> (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '<) (< (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '!) (not operand1))
      (else (error "unknown operator:" (operator expr))) )))
      
; A method to compute the value for all integer computations

(define value
  (lambda (expr state)
    (cond
      ((number? expr) (inexact->exact expr))    ; the base case is just returns the value if it's a number
      ((not (list? expr)) (getValue expr state)) ;second base case where getting variable value
      ((eq? (operator expr) '+) (+ (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '-) (- (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '*) (* (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '/) (quotient (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '%) (remainder (value (operand1 expr) state) (value (operand2 expr) state)))  
      ((eq? (operator expr) 'return) (value (operand1 expr) state))
      ((eq? (operator expr) '=) (setVar (value operand1) state))
      ((eq? (operator expr) 'if) (ifEval expr))
      ((eq? (operator expr) 'while) (whileEval expr))
      )))

; A function to evaluate the different possiblities in an if statement or if else statement
(define ifEval
  (lambda (expr state)
    (cond
      ((and (eq? (operator expr) 'if) (boolean (operand1 expr) state)) (statement (operand2 expr) state));if succeeds
      ((not (eq? (operator expr) 'if)) (statement expr state)); else
      ((null? (cdddr expr)) 0); last if fails no else
      (else (ifEval (cadddr expr) state)); else if
    )))

; A function to evaluate while loops

(define whileEval
  (lambda (expr state)
      ((boolean (operand1 expr)) (value (operand2 expr) state))
      ((boolean (operand1 expr)) (whileEval expr state))
    ))
