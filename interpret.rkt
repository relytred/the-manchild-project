#|
Jeffrey Noymer & Tyler Mayle
jan88 & tcm45
Project 1
|#


(load "simpleParser.scm")
(load "control.rkt")
(require racket/trace)


; A method to take in an external string file and read it into our interpreter

(define interpret
  (lambda (expr)
    (call/cc
       (lambda (return)
         (runTree (parser expr) newstate return "null") ))))

; A method to determine whether or not the expr is the beginning of a block

(define block?
  (lambda (expr)
    (eq? (car expr) 'begin)))

; A method that facilitates all of the activity of the program

(define runTree
  (lambda (expr state return break)
    (cond
      ((null? expr) state)
      ((block? (car expr)) (runTree (cdr expr) (block (cdar expr) (addSubstate state) return break) return break)) 
      (else (runTree (cdr expr) (statement (car expr) state return break) return break)) )))

; Helper methods to determine which element is an operator or an operand in the statemnt

(define operator car)
(define operand1 cadr)
(define operand2 caddr)

; A function to facilitate the handling of substate blocks

(define block
  (lambda (exprs state return break)
    (cond
      ((null? exprs) (removeSubstate state))
      ((block? exprs) (block (cdr exprs) (block (car exprs)(addSubstate state) return break) return break))
      (else (block (cdr exprs) (statement (car exprs) state return break) return break)) )))  

; A function to evaluate different types of statements

(define statement
  (lambda (expr state return break)
    (cond
      ((eq? (operator expr) 'return) (return (returnHelp expr state)))
      ((and (eq? (operator expr) 'var) (null? (cddr expr))) (declareVariable (operand1 expr) "null" state))
      ((eq? (operator expr) 'var) (declareVariable (operand1 expr) (value (operand2 expr) state) state))
      ((eq? (operator expr) '=) (assignVariable (operand1 expr) (value (operand2 expr) state) state))
      ((eq? (operator expr) 'if) (ifEval expr state return break))
      ((eq? (operator expr) 'while) (call/cc
                                     (lambda (breakPoint)
                                       (whileEval expr state return breakPoint))))
      ((eq? (operator expr) 'break) (breakEval state break))
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
  (lambda (expr state return break)
    (cond
      ((and (eq? (operator expr) 'if) (boolean (operand1 expr) state)) (runTree (cons (operand2 expr) '()) state return break));if succeeds
      ((not (eq? (operator expr) 'if)) (runTree (cons expr '()) state return break)); else
      ((null? (cdddr expr)) state); last if fails no else
      (else (ifEval (cadddr expr) state return break)); else if
    )))

; A function to evaluate while loops

(define whileEval
  (lambda (expr state return break)
    (cond
      ((boolean (operand1 expr) state) (whileEval expr (runTree (cons (operand2 expr) '()) state return break) return break))
      (else state)
    )))

(define canBreak
  (lambda (break)
    (not (eq? break "null"))))

(define breakEval
  (lambda (state break)
    (cond
      ((canBreak break) (break (removeSubstate state)))
      (else (error "Illegal use of break statement")) )))