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
       (runTree (parser expr) newstate return "null" "null" "null") ))))

; A method to determine whether or not the expr is the beginning of a block

(define block?
  (lambda (expr)
    (eq? (car expr) 'begin)))

(define try?
  (lambda (expr)
    (or (eq? (car expr) 'try))))

; A method that facilitates all of the activity of the program

(define runTree
  (lambda (expr state return break cont throw)
    (cond
      ((null? expr) state)
      ((block? (car expr)) (runTree (cdr expr) (block (cdar expr) (addSubstate state) return break cont throw) return break cont throw)) 
      (else (runTree (cdr expr) (statement (car expr) state return break cont throw) return break cont throw)) )))

; Helper methods to determine which element is an operator or an operand in the statemnt

(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

; A function to facilitate the handling of substate blocks

(define block
  (lambda (exprs state return break cont throw)
    (cond
      ((null? exprs) (removeSubstate state))
      ((block? exprs) (block (cdr exprs) (block (car exprs)(addSubstate state) return break cont throw) return break cont throw))
      (else (block (cdr exprs) (statement (car exprs) state return break cont throw) return break cont throw)) )))  

; A function to evaluate different types of statements

(define statement
  (lambda (expr state return break cont throw)
    (cond
      ((eq? (operator expr) 'return) (return (returnHelp expr state)))
      ((and (eq? (operator expr) 'var) (null? (cddr expr))) (declareVariable (operand1 expr) "null" state))
      ((eq? (operator expr) 'var) (declareVariable (operand1 expr) (value (operand2 expr) state) state))
      ((eq? (operator expr) '=) (assignVariable (operand1 expr) (value (operand2 expr) state) state))
      ((eq? (operator expr) 'if) (ifEval expr state return break cont throw))
      ((eq? (operator expr) 'while) (call/cc
                                     (lambda (breakPoint)
                                       (whileEval expr state return breakPoint cont throw))))
      ((eq? (operator expr) 'break) (breakEval state break) throw)
      ((eq? (operator expr) 'continue) (continueEval state cont) throw)
      ((eq? (operator expr) 'try) (catchEval expr (tryEval expr state return break cont throw) return break cont throw))
      ((eq? (operator expr) 'throw) (throwEval expr state throw))
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
  (lambda (expr state return break cont throw)
    (cond
      ((and (eq? (operator expr) 'if) (boolean (operand1 expr) state)) (runTree (cons (operand2 expr) '()) state return break cont throw));if succeeds
      ((not (eq? (operator expr) 'if)) (runTree (cons expr '()) state return break cont throw)); else
      ((null? (cdddr expr)) state); last if fails no else
      (else (ifEval (cadddr expr) state return break cont throw)); else if
      )))

; A function to evaluate while loops

(define whileEval
  (lambda (expr state return break cont throw)
    (cond
      ((boolean (operand1 expr) state)
       (whileEval expr
                  (call/cc
                   (lambda (continue)
                     (runTree (cons (operand2 expr) '()) state return break continue throw)))
                  return break cont throw))
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

(define hasCatch
  (lambda (expr)
    (not (null? (operand2 expr)))
    ))

(define tryEval
  (lambda (expr state return break cont throw)
    (call/cc
     (lambda (catch)
       (runTree (cadr expr) state return break cont catch))) ))

(define throwEval
  (lambda (expr state throw)
    (throw (append state (cons (value (operand1 expr) state) '())) )))

(define thrown?
  (lambda (state)
    (eq? (length throwValue) 4)))

(define catchStatement
 (lambda (expr)
   (caddr expr)))

(define catchVar
  (lambda (expr)
    (caadr (catchStatement expr)) ))

(define catchBody
  (lambda (expr)
    (caddr (catchStatement expr)) ))

(define catchState
  (lambda (var throwValue state)
    (declareVariable var (value throwValue state) (addSubstate state))))

(define catchEval
 (lambda (expr state return break cont throw throwValue)
   (cond
     ((not (thrown? throwValue)) throwValue) 
     ((null? (catchStatement expr)) (error "Illegal throw"))
     (else (runTree (catchBody expr) (catchState (catchVar expr) throwValue state) return break cont throw)) )))
