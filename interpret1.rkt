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
    (value (car (parser expr)))
    ))


; Defining our operators to get operands and operators

(define operator car)
(define operand1 cadr)
(define operand2 caddr)


(define value
  (lambda (expr)
    (cond
      ((number? expr) (inexact->exact expr))    ; the base case is just returns the value if it's a number
      ((eq? (operator expr) '+) (+ (value (operand1 expr)) (value (operand2 expr))))
      ((eq? (operator expr) '-) (- (value (operand1 expr)) (value (operand2 expr))))
      ((eq? (operator expr) '*) (* (value (operand1 expr)) (value (operand2 expr))))
      ((eq? (operator expr) '=) (setVar (value operand1)))
      ((eq? (operator expr) '/) (quotient (value (operand1 expr)) (value (operand2 expr))))
      ((eq? (operator expr) '%) (remainder (value (operand1 expr)) (value (operand2 expr))))
      ((eq? (operator expr) '&&) (and (value (operand1 expr)) (value (operand2 expr))))   
      ((eq? (operator expr) '==) (eq? (value (operand1 expr)) (value (operand2 expr))))
      ((eq? (operator expr) '!=) (not (eq? (value (operand1 expr)) (value (operand2 expr)))))
      ((eq? (operator expr) '<=) (<= (value (operand1 expr)) (value (operand2 expr))))
      ((eq? (operator expr) '>=) (>= (value (operand1 expr)) (value (operand2 expr))))
      ((eq? (operator expr) '>) (> (value (operand1 expr)) (value (operand2 expr))))
      ((eq? (operator expr) '<) (< (value (operand1 expr)) (value (operand2 expr))))
      ((eq? (operator expr) '!) (not operand1))
      ((eq? (operator expr) 'return) (value (operand1 expr)))
      ;((eq? (operator expr) 'var) (varDeclare (operand1 expr)))
      ((eq? (operator expr) 'if) (ifEval expr))
      ((eq? (operator expr) 'while) (whileEval expr))
      ;((eq? (operator expr) '|| (or (value (operand1 expr)) (value (operand2 expr)))))
      ;((declared? expr) (getValue expr))
      (else (error "unknown operator:" (operator expr))) )))

; A function to evaluate the different possiblities in an if statement or if else statement

(define ifEval
  (lambda (expr)
    (cond
      ((value (operand1 expr)) (value (operand2 expr)))
      (else (value (operand2 expr)))
    )))

; A function to evaluate while loops

(define whileEval
  (lambda (expr)
    (cond
      ((operand1 expr) (operand2 expr) (whileEval expr))
    )))