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
    (statement (car (parser expr)) '(()()))
    ))


; Defining our operators to get operands and operators

(define operator car)
(define operand1 cadr)
(define operand2 caddr)



(define statement
  (lambda (expr state)
    (cond
      ((eq? (operator expr) 'return) (value (operand1 expr) state))
      ((eq? (operator expr) 'var) (declareVariable (operand1 expr) state))
      ((eq? (operator expr) '=) (assignVariable (operand1 expr) (operand2 expr) state))
      ((eq? (operator expr) 'if) (ifEval expr state))
      ((eq? (operator expr) 'while) (whileEval expr)) )))



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
      

(define value
  (lambda (expr state)
    (cond
      ((number? expr) (inexact->exact expr))    ; the base case is just returns the value if it's a number
      ((eq? (operator expr) '+) (+ (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '-) (- (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '*) (* (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '/) (quotient (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '%) (remainder (value (operand1 expr) state) (value (operand2 expr) state)))  
      ((eq? (operator expr) 'return) (value (operand1 expr) state))
      ((eq? (operator expr) '=) (setVar (value operand1) state))
      ((eq? (operator expr) 'if) (ifEval expr))
      ((eq? (operator expr) 'while) (whileEval expr))
      (else (boolean expr state)) )))

; A function to evaluate the different possiblities in an if statement or if else statement
(define ifEval
  (lambda (expr state)
    (cond
      ((and (eq? (operator expr) 'if) (boolean (operand1 expr) state)) (value (operand2 expr) state));if succeeds
      ((not (eq? (operator expr) 'if)) (statement expr state)); else
      ((null? (cdddr expr)) 0); last if fails no else
      (else (ifEval (cadddr expr) state)); else if
    )))

; A function to evaluate while loops

(define whileEval
  (lambda (expr)
    (cond
      ((operand1 expr) (operand2 expr) (whileEval expr))
    )))