#|
Jeffrey Noymer & Tyler Mayle
jan88 & tcm45
Project 2
|#

(load "classParser.scm")
(load "control.rkt")
;(load "test.rkt")
(require racket/trace)


; A method to take in an external string file and read it into our interpreter

(define interpret
  (lambda (expr)
    (call/cc
     (lambda (return)
       (runMain newstate (initClasses (parser expr) '()) return) ))))
     ;  (runMain (runTree (parser expr) newstate "null" "null" "null" "null") return) ))))

(define initClasses
  (lambda (expr classes)
    (cond
      ((null? expr) classes)
      (else (initClasses (next expr) (initClass (first expr) classes))))))

(define initClass
  (lambda (expr classes)
    (addClass (operand1 expr) (parseParent (operand2 expr)) (runTree (operand3 expr) newstate '() "null" "null" "null" "null") classes)))

(define runMain
  (lambda (state classes return func)
    (runFunc (getFunctionBody name state) (createFunctionState name (getParamValues params state classes return break cont throw) state))
       (functionCallEval 'main '() state return "null" "null" "null") ))

(define parseParent
  (lambda (expr)
    (cond
      ((null? expr) "null")
      (else (operand1 expr)))))
  
;      ((eq? (operator expr) 'class) (addClass (operand1 expr) (operand2 expr) (runTree (operand3 expr) state return break cont throw)))

(define containMain?
  (lambda (classFuncs)
    (cond
      ((null? classFuncs) #f)
      ((eq? (operator (first classFuncs)) 'main) (first classFunc))
      (else (containMain (pop classFuncs))) )))

(define classContents caddr) 
    
(define findMain
  (lambda (classes)
    (cond
      ((not (containMain? (getFunctions (classContents(first classes))))) (findMain (pop classes)))
      (else (containMain? (getFunctions (classContents(first classes))))) )))
    
(define runFunc
  (lambda (expr state classes throw)
    (call/cc
     (lambda (return)
       (runTree expr state classes return "null" "null" throw) ))))

; A method to determine whether or not the expr is the beginning of a block

(define block?
  (lambda (expr)
    (eq? (car expr) 'begin)))

(define try?
  (lambda (expr)
    (or (eq? (car expr) 'try))))

; A method that facilitates all of the activity of the program

(define runTree
  (lambda (expr state classes return break cont throw)
    (cond
      ((null? expr) state)
      ((block? (car expr)) (runTree (cdr expr) (block (cdar expr) (addSubstate state) classes return break cont throw) return break cont throw)) 
      (else (runTree (cdr expr) (statement (car expr) state classes return break cont throw) classes return break cont throw)) )))

; Helper methods to determine which element is an operator or an operand in the statemnt

(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)
(define params cddr)
(define next cdr)

; A function to facilitate the handling of substate blocks

(define block
  (lambda (exprs state classes return break cont throw)
    (cond
      ((null? exprs) (removeSubstate state))
      ((block? exprs) (block (cdr exprs) (block (car exprs)(addSubstate state) classes return break cont throw) return break cont throw))
      (else (block (cdr exprs) (statement (car exprs) state classes return break cont throw) classes return break cont throw)) )))  

; A function to evaluate different types of statements

(define statement
  (lambda (expr state classes return break cont throw)
    (cond
      ((eq? (operator expr) 'return) (return (returnHelp expr state classes return break cont throw)))
      ((and (eq? (operator expr) 'var) (null? (cddr expr))) (declareVariable (operand1 expr) "null" state))
      ((eq? (operator expr) 'var) (declareVariable (operand1 expr) (value (operand2 expr) state classes return break cont throw) state))
      ((eq? (operator expr) '=) (assignVariable (operand1 expr) (value (operand2 expr) state classes return break cont throw) state))
      ((or (eq? (operator expr) 'function) (eq? (operator expr) 'static-function)) (addFunction (operand1 expr) (operand2 expr) (operand3 expr) state))
      ((eq? (operator expr) 'funcall) (begin (functionCallEval (operand1 expr) (params expr) state classes return break cont throw) state))
      ((eq? (operator expr) 'if) (ifEval expr state classes return break cont throw))
      ((eq? (operator expr) 'while) (call/cc
                                     (lambda (breakPoint)
                                       (whileEval expr state classes return breakPoint cont throw))))
      ((eq? (operator expr) 'break) (breakEval state break) throw)
      ((eq? (operator expr) 'continue) (continueEval state cont) throw)
      ((eq? (operator expr) 'try) (finallyEval expr (catchEval expr (tryEval expr state classes return break cont throw)
                                                               classes return break cont throw)
                                               classes return break cont throw))
      ((eq? (operator expr) 'throw) (throwEval expr state classes throw))
      )))   
; A method to evaluate all of the boolean operations and update their states

(define boolean
  (lambda (expr state return classes break cont throw)
    (cond
      ((and (not (list? expr)) (eq? expr 'true)) #t)
      ((and (not (list? expr)) (eq? expr 'false)) #f)
      ((eq? (operator expr) '&&) (and (value (operand1 expr) state classes return break cont throw) (value (operand2 expr) state classes return break cont throw)))
      ((eq? (operator expr) '||) (or (value (operand1 expr) state classes return break cont throw) (value (operand2 expr) state classes return break cont throw)))
      ((eq? (operator expr) '==) (eq? (value (operand1 expr) state classes return break cont throw) (value (operand2 expr) state classes return break cont throw)))
      ((eq? (operator expr) '!=) (not (eq? (value (operand1 expr) state classes return break cont throw) (value (operand2 expr) state classes return break cont throw))))
      ((eq? (operator expr) '<=) (<= (value (operand1 expr) state classes return break cont throw) (value (operand2 expr) state classes return break cont throw)))
      ((eq? (operator expr) '>=) (>= (value (operand1 expr) state classes return break cont throw) (value (operand2 expr) state classes return break cont throw)))
      ((eq? (operator expr) '>) (> (value (operand1 expr) state classes return break cont throw) (value (operand2 expr) state classes return break cont throw)))
      ((eq? (operator expr) '<) (< (value (operand1 expr) state classes return break cont throw) (value (operand2 expr) state classes return break cont throw)))
      ((eq? (operator expr) '!) (not (value (operand1 expr) state classes return break cont throw)))
      ((eq? (operator expr) 'funcall) (functionCallEval (operand1 expr) (params expr) state classes return break cont throw))
      
      (else (error "unknown operator:" (operator expr))) )))
      
; A method to compute the value for all integer computations

(define value
  (lambda (expr state classes return break cont throw)
    (cond
      ((number? expr) (inexact->exact expr))    ; the base case is just returns the value if it's a number
      ((and (not (list? expr)) (eq? expr 'true)) #t)
      ((and (not (list? expr)) (eq? expr 'false)) #f)
      ((not (list? expr)) (getValue expr state)) ;second base case where getting variable value
      ((eq? (operator expr) '+) (+ (value (operand1 expr) state classes return break cont throw) (value (operand2 expr) state classes return break cont throw)))
      ((eq? (operator expr) '-) (subEval expr state classes return break cont throw))
      ((eq? (operator expr) '*) (* (value (operand1 expr) state classes return break cont throw) (value (operand2 expr) state classes return break cont throw)))
      ((eq? (operator expr) '/) (quotient (value (operand1 expr) state classes return break cont throw) (value (operand2 expr) state classes return break cont throw)))
      ((eq? (operator expr) '%) (remainder (value (operand1 expr) state classes return break cont throw) (value (operand2 expr) state classes return break cont throw)))
      ((eq? (operator expr) 'new) (newEval (operand1 expr) state classes return break cont throw))
      (else (boolean expr state classes return break cont throw))
      )))

; A function assist in returning values, specifically turning #t -> true and #f -> false
;(value (cadar expr) state)

(define returnHelp
  (lambda (expr state classes return break cont throw)
    ;(display (value (cadar expr) state))
    (cond
      ((eq? (value (operand1 expr) state classes return break cont throw) #t) 'true)
      ((eq? (value (operand1 expr) state classes return break cont throw) #f) 'false)
      (else (value (operand1 expr) state classes return break cont throw))
      )))

; A function to evaluate the - symbol works as a negative sign and as an operator

(define subEval
  (lambda (expr state classes return break cont throw)
    (cond
      ((null? (cddr expr)) (- 0 (value (operand1 expr) state classes return break cont throw)))
      (else (- (value (operand1 expr) state classes return break cont throw) (value (operand2 expr) state classes return break cont throw)))
      )))

; A function to evaluate the different possiblities in an if statement or if else statement
(define ifEval
  (lambda (expr state classes return break cont throw)
    (cond
      ((and (eq? (operator expr) 'if) (boolean (operand1 expr) state classes return break cont throw)) (runTree (cons (operand2 expr) '()) state classes return break cont throw));if succeeds
      ((not (eq? (operator expr) 'if)) (runTree (cons expr '()) state classes return break cont throw)); else
      ((null? (cdddr expr)) state); last if fails no else
      (else (ifEval (cadddr expr) state classes return break cont throw)); else if
      )))

; A function to evaluate while loops

(define whileEval
  (lambda (expr state classes return break cont throw)
    (cond
      ((boolean (operand1 expr) state classes return break cont throw)
       (whileEval expr
                  (call/cc
                   (lambda (continue)
                     (runTree (cons (operand2 expr) '()) state classes return break continue throw)))
                  classes return break cont throw))
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

; A function to handle continue statements

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

; A function to evaluate the try block

(define tryEval
  (lambda (expr state classes return break cont throw)
    (list
     (call/cc
      (lambda (catch)
        (runTree (cadr expr) (addSubstate state) classes return break cont catch)))
     state) ))

; A function to handle the throw event

(define throwEval
  (lambda (expr state classes throw)
    (cond
      ((eq? throw "null") (error "Illegal throw"))
      (else (throw (value (operand1 expr) state classes "null" "null" "null" "null"))) )))

; A function determining whether it has been thrown

(define thrown?
  (lambda (state)
    (not (list? (first state)))))

; A function to get the catch statement

(define catchStatement
 (lambda (expr)
   (caddr expr)))

; A function to get the variable of the catch

(define catchVar
  (lambda (expr)
    (caadr (catchStatement expr)) ))

; A function to get the body of the catch

(define catchBody
  (lambda (expr)
    (caddr (catchStatement expr)) ))

; A function to handle the catch state

(define catchState
  (lambda (var val state classes)
    (declareVariable var (value val state classes "null" "null" "null" "null") (addSubstate state))))

; A function to evaluate the catch statement

(define catchEval
 (lambda (expr state classes return break cont throw)
   (cond
     ((not (thrown? state)) (first state))
     (else (runTree (catchBody expr) (catchState (catchVar expr) (first state) (operand1 state) classes) classes return break cont throw)) )))

; A function to get the finally statement

(define finallyStatement
  (lambda (expr)
    (cadddr expr)) )

; A function to handle the body of the finally statement)

(define finallyBody
  (lambda (expr)
    (cadr (finallyStatement expr)) ))

; A function to handle the finally state

(define finallyState
  (lambda (state)
    (addSubstate (removeSubstate state)) ))

; A function to evaluate finally blocks

(define finallyEval
  (lambda (expr state classes return break cont throw)
    (cond
      ((null? (finallyStatement expr)) (removeSubstate state))
      (else (removeSubstate (runTree (finallyBody expr) (finallyState state) classes return break cont throw))) )))

(define getParamValues
  (lambda (params state classes return break cont throw)
    (cond
      ((null? params) '())
      ((and (list? (operator params)) (eq? (operator (operator params)) 'funcall)) (cons (statement expr state classes return break cont throw) (getParamValues (cdr params) state classes return break cont throw)))
      (else (cons (value (first params) state classes return break cont throw) (getParamValues (cdr params) state classes return break cont throw))) )))
    
(define functionCallEval
  (lambda (name params state classes return break cont throw)
    (runFunc (getFunctionBody name state) (createFunctionState name (getParamValues params state classes return break cont throw) state) throw)))
    
(define newEval
  (lambda (class state return break cont throw)
    (getNewClass class (classes state))))


;Class functions...
(define getState cadr)

(define constructClassState
  (lambda (state newState)
    (list (first state) newState)))