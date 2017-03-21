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
    (call/cc
       (lambda (return)
         (runTree (parser expr) newstate return) ))))

(define block?
  (lambda (expr)
    (eq? (car expr) 'begin)))

(define runTree
  (lambda (expr state return)
    (cond
      ((null? expr) state)
      ((block? (car expr)) (runTree (cdr expr) (block (cdar expr) (addSubstate state) return) return)) 
      (else (runTree (cdr expr) (statement (car expr) state return) return)) )))

; Helper methods to determine which element is an operator or an operand in the statemnt

(define operator car)
(define operand1 cadr)
(define operand2 caddr)

(define block
  (lambda (exprs state return)
    (cond
      ((null? exprs) (removeSubstate state))
      ((block? exprs) (block (cdr exprs)(addSubstate state) ))
      (else (block (cdr exprs) (statement (car exprs) state))) )))  

(define statement
  (lambda (expr state return)
    (cond
      ((eq? (operator expr) 'return) (return (returnHelp expr state)))
      ((and (eq? (operator expr) 'var) (null? (cddr expr))) (declareVariable (operand1 expr) "null" state))
      ((eq? (operator expr) 'var) (declareVariable (operand1 expr) (value (operand2 expr) state) state))
      ((eq? (operator expr) '=) (assignVariable (operand1 expr) (value (operand2 expr) state) state))
      ((eq? (operator expr) 'if) (ifEval expr state return))
      ((eq? (operator expr) 'while) (whileEval expr state return))
      )))   
; A method to evaluate all of the boolean operations and update their states

(define boolean
  (lambda (expr state)
    (cond
      ((and (not (list? expr)) (eq? expr 'true)) #t)
      ((and (not (list? expr)) (eq? expr 'false)) #f)
      ((eq? (operator expr) '&&) (and (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '||) (or (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '==) (eq? (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '!=) (not (eq? (value (operand1 expr) state) (value (operand2 expr) state))))
      ((eq? (operator expr) '<=) (<= (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '>=) (>= (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '>) (> (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '<) (< (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '!) (not (value (operand1 expr) state)))
      
      (else (error "unknown operator:" (operator expr))) )))
      
; A method to compute the value for all integer computations

(define value
  (lambda (expr state)
    (cond
      ((number? expr) (inexact->exact expr))    ; the base case is just returns the value if it's a number
      ((and (not (list? expr)) (eq? expr 'true)) #t)
      ((and (not (list? expr)) (eq? expr 'false)) #f)
      ((not (list? expr)) (getValue expr state)) ;second base case where getting variable value
      ((eq? (operator expr) '+) (+ (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '-) (subEval expr state))
      ((eq? (operator expr) '*) (* (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '/) (quotient (value (operand1 expr) state) (value (operand2 expr) state)))
      ((eq? (operator expr) '%) (remainder (value (operand1 expr) state) (value (operand2 expr) state)))  
      (else (boolean expr state))
      )))

; A function assist in returning values, specifically turning #t -> true and #f -> false
;(value (cadar expr) state)

(define returnHelp
  (lambda (expr state)
    ;(display (value (cadar expr) state))
    (cond
      ((eq? (value (operand1 expr) state) #t) 'true)
      ((eq? (value (operand1 expr) state) #f) 'false)
      (else (value (operand1 expr) state))
      )))

; A function to evaluate the - symbol works as a negative sign and as an operator

(define subEval
  (lambda (expr state)
    (cond
      ((null? (cddr expr)) (- 0 (value (operand1 expr) state)))
      (else (- (value (operand1 expr) state) (value (operand2 expr) state)))
      )))

; A function to evaluate the different possiblities in an if statement or if else statement
(define ifEval
  (lambda (expr state return)
    (cond
      ((and (eq? (operator expr) 'if) (boolean (operand1 expr) state)) (runTree (cons (operand2 expr) '()) state return));if succeeds
      ((not (eq? (operator expr) 'if)) (runTree (cons expr '()) state return)); else
      ((null? (cdddr expr)) state); last if fails no else
      (else (ifEval (cadddr expr) state return)); else if
    )))

; A function to evaluate while loops

(define whileEval
  (lambda (expr state return)
    (cond
      ((boolean (operand1 expr) state) (whileEval expr (runTree (cons (operand2 expr) '()) state return) return))
      (else state)
    )))

(define test
  (lambda (x)
    (call/cc
     (lambda (return)
       (other x return) ))))

(define other
  (lambda (x return)
    (cond
      ((> x 5) (* x (other (- x 1) return)))
      (else (return x)))))