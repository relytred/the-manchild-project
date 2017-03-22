#|
Jeffrey Noymer & Tyler Mayle
jan88 & tcm45
Project 2
|#

(load "simpleParser.scm")
(load "control.rkt")
;(load "test.rkt")
(require racket/trace)


; A method to take in an external string file and read it into our interpreter

(define interpret
  (lambda (expr)
    (call/cc
       (lambda (return)
         (runTree (parser expr) newstate return "null" "null") ))))

; A method to determine whether or not the expr is the beginning of a block

(define block?
  (lambda (expr)
    (eq? (car expr) 'begin)))

; A method that facilitates all of the activity of the program

(define runTree
  (lambda (expr state return break cont)
    (cond
      ((null? expr) state)
      ((block? (car expr)) (runTree (cdr expr) (block (cdar expr) (addSubstate state) return break cont) return break cont)) 
      (else (runTree (cdr expr) (statement (car expr) state return break cont) return break cont)) )))

; Helper methods to determine which element is an operator or an operand in the statemnt

(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

; A function to facilitate the handling of substate blocks

(define block
  (lambda (exprs state return break cont)
    (cond
      ((null? exprs) (removeSubstate state))
      ((block? exprs) (block (cdr exprs) (block (car exprs)(addSubstate state) return break cont) return break cont))
      (else (block (cdr exprs) (statement (car exprs) state return break cont) return break cont)) )))  

; A function to evaluate different types of statements

(define statement
  (lambda (expr state return break cont)
    (cond
      ((eq? (operator expr) 'return) (return (returnHelp expr state)))
      ((and (eq? (operator expr) 'var) (null? (cddr expr))) (declareVariable (operand1 expr) "null" state))
      ((eq? (operator expr) 'var) (declareVariable (operand1 expr) (value (operand2 expr) state) state))
      ((eq? (operator expr) '=) (assignVariable (operand1 expr) (value (operand2 expr) state) state))
      ((eq? (operator expr) 'if) (ifEval expr state return break cont))
      ((eq? (operator expr) 'while) (call/cc
                                     (lambda (breakPoint)
                                       (whileEval expr state return breakPoint cont))))
      ((eq? (operator expr) 'break) (breakEval state break))
      ((eq? (operator expr) 'continue) (continueEval state cont))
      ((eq? (operator expr) 'try) (tryEval expr state return break cont))
      ((eq? (operator expr) 'throw) (goToCatch expr state return break eval #f))
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
  (lambda (expr state return break cont)
    (cond
      ((and (eq? (operator expr) 'if) (boolean (operand1 expr) state)) (runTree (cons (operand2 expr) '()) state return break cont));if succeeds
      ((not (eq? (operator expr) 'if)) (runTree (cons expr '()) state return break cont)); else
      ((null? (cdddr expr)) state); last if fails no else
      (else (ifEval (cadddr expr) state return break cont)); else if
    )))

; A function to evaluate while loops

(define whileEval
  (lambda (expr state return break cont)
    (cond
      ((boolean (operand1 expr) state)
       (whileEval expr
                  (call/cc
                   (lambda (continue)
                     (runTree (cons (operand2 expr) '()) state return break continue)))
                     return break cont))
      (else state)
    )))

; A function to determine whether or not a method can break

(define inLoop
  (lambda (continuation)
    (not (eq? continuation "null"))))

; A function to evaluate the break statement

(define breakEval
  (lambda (state break)
    (cond
      ((inLoop break) (break (removeSubstate state)))
      (else (error "Illegal use of break statement")) )))

(define continueEval
  (lambda (state cont)
    (cond
      ((inLoop cont) (cont state))
      (else (error "Illegal use of continue statement")) )))

  ; A function to evaluate try catch blocks

(define tryEval
  (lambda (expr state return break cont)
    (cond
      ((and () (statement (operand1 expr) state return break cont)))
      ((and () (statement (operand2 expr) state return break cont)))
      ;((caught) (catchEval expr state return break cont))
      (null?(statement (operand2 expr) state return break cont))
      )))

#|(catchEval
 (lambda (expr state return break cont)
   (cond
     ((statement expr state return break cont))
     )))
|#