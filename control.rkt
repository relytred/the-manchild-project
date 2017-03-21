#|
Jeffrey Noymer & Tyler Mayle
jan88 & tcm45
Project 1
|#

(load "simpleParser.scm")
(require racket/trace)
;state functions---------------------------------------------------------------
(define newstate '(() ()))

; A fuction to get the variables of a given state

(define getVariables
  (lambda (state)
    (car state)))

; A function to get the value of a given state

(define getValues
  (lambda (state)
    (cadr state)))

; A function to get the substate of given state

(define substate
  (lambda (state)
    (caddr state)))

; A function to check if a state has a substate

(define hasSubstate
  (lambda (state)
    (eq? (length state) 3) ))

; A function to add a substate to a given state

(define addSubstate
  (lambda (state)
    (cond
      ((hasSubstate state) (constructSubstate (getVariables state) (getValues state) (addSubstate (substate state))))
      (else (constructSubstate (getVariables state) (getValues state) newstate)) )))

; A function to remove a substate from the given state

(define removeSubstate
  (lambda (state)
    (cond
      ((hasSubstate (substate state)) (constructSubstate (getVariables state) (getValues state) (removeSubstate (substate state))))
      (else (removeLast state)) )))

; A function to get the value of a given state

(define getValue
  (lambda (var state)
    (cond
      ((not (declared? var state)) (error "variable not declared:" var))
      ((eq? (getMatch var state) "null") (error "variable not initialized" var))
      (else (getMatch var state)) )))

; A function to find the matching value for the variable in a given state

(define getMatch
  (lambda (var state)
    (cond
      ((null? (getVariables state)) (getMatch var (substate state)))
      ((eq? var (car (getVariables state))) (car (getValues state)))
      (else (getMatch var (constructState (cdr (getVariables state)) (cdr (getValues state))  state))) )))  

; A fuction to determine weather or not a variable has been declared yet

(define declared?
  (lambda (var state)
    (cond
      ((include? var (getVariables state)) #t)
      ((hasSubstate state) (declared? var (substate state)))
      (else #f) )))

; A function to declare a variable and add it to a state

(define declareVariable
  (lambda (var value state)
    (cond
      ((declared? var state) (error "redifining:" var))
      (else (addVariable var value state)))))

; A function to assign a variable a value in its state

(define assignVariable
  (lambda (var value state)
    (cond
      ((not (declared? var state)) (error "variable not declared:" var))
      (else (replaceVariable var value state))) ))

; A function to replace a variable

(define replaceVariable
  (lambda (var value state)
    (cond
      ((include? var (getVariables state)) (constructState
                                            (cons var (getVariables (removeMatch var (getVariables state) (getValues state))))
                                            (cons value (getValues (removeMatch var (getVariables state) (getValues state))))
                                            state))                                            
      ((hasSubstate state) (constructSubstate (getVariables state) (getValues state) (replaceVariable var value (substate state))))
      (else state) )))

; A function to add a variable to a state

(define addVariable
  (lambda (var value state)
    (cond
      ((hasSubstate state) (constructSubstate (getVariables state) (getValues state) (addVariable var value (substate state))))
      (else (constructState (cons var (getVariables state)) (cons value (getValues state)) state)) )))

; A function to construct a state

(define constructState
  (lambda (vars values state)
    (cond
      ((hasSubstate state) (list vars values (substate state)))
      (else (list vars values)) )))

; A function to construct a substate

(define constructSubstate
  (lambda (variables values subState)
    (append (list variables values subState))))


;utility functions------------------------------------------------------------------

; A function to return weather an element is included in a given list

(define include?
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((eq? x (car l)) #t)
      (else (include? x (cdr l))))))

; A function remove the last of a list

(define removeLast
  (lambda (l)
    (cond
      ((eq? (length l) 1) '())
      (else (cons (car l) (removeLast (cdr l)))))))

; A function to remove an element and its corresponding value from two matched lists

(define removeMatch
  (lambda (x l1 l2)
    (cond
      ((null? l1) '(() ()))
      ((eq? x (car l1)) (list (cdr l1) (cdr l2))) 
      (else (list (cons (car l1) (car (removeMatch x (cdr l1) (cdr l2)))) (cons (car l2) (cadr (removeMatch x (cdr l1) (cdr l2)))))))))
         
