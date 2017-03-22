#|
Jeffrey Noymer & Tyler Mayle
jan88 & tcm45
Project 1
|#

(load "simpleParser.scm")
(require racket/trace)
;state functions---------------------------------------------------------------
(define newstate '(() ()))
(define pop cdr)
(define push cons)

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
      ((eq? var (first (getVariables state))) (first (getValues state)))
      (else (getMatch var (constructState (pop (getVariables state)) (pop (getValues state))  state))) )))  

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
                                            (push var (getVariables (removeMatch var (getVariables state) (getValues state))))
                                            (push value (getValues (removeMatch var (getVariables state) (getValues state))))
                                            state))                                            
      ((hasSubstate state) (constructSubstate (getVariables state) (getValues state) (replaceVariable var value (substate state))))
      (else state) )))

; A function to add a variable to a state

(define addVariable
  (lambda (var value state)
    (cond
      ((hasSubstate state) (constructSubstate (getVariables state) (getValues state) (addVariable var value (substate state))))
      (else (constructState (push var (getVariables state)) (push value (getValues state)) state)) )))

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
      ((eq? x (first l)) #t)
      (else (include? x (pop l))))))

; A function remove the last of a list

(define removeLast
  (lambda (l)
    (cond
      ((eq? (length l) 1) '())
      (else (push (first l) (removeLast (pop l)))))))

; A function to remove an element and its corresponding value from two matched lists

(define removeMatch
  (lambda (x l1 l2)
    (cond
      ((null? l1) '(() ()))
      ((eq? x (first l1)) (list (pop l1) (pop l2))) 
      (else (list (push (first l1) (first (removeMatch x (pop l1) (pop l2)))) (push (first l2) (cadr (removeMatch x (pop l1) (pop l2)))))))))
         
