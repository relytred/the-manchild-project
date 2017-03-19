(load "simpleParser.scm")
(require racket/trace)
;state functions---------------------------------------------------------------
(define newstate '(() ()))
  
(define substate
  (lambda (state)
    (caddr state)))

(define hasSubstate
  (lambda (state)
    (eq? (length state) 3) ))

(define addSubstate
  (lambda (state)
    (cond
      ((hasSubstate state) (constructSubstate (getVariables state) (getValues state) (addSubstate (substate state))))
      (else (constructSubstate (getVariables state) (getValues state) newstate)) )))

(define removeSubstate
  (lambda (state)
    (cond
      ((hasSubstate (substate state)) (constructSubstate (getVariables state) (getValues state) (removeSubstate (substate state))))
      (else (removeLast state)) )))

(define getVariables
  (lambda (state)
    (car state)))

(define getValues
  (lambda (state)
    (cadr state)))

(define getValue
  (lambda (var state)
    (cond
      ((not (declared? var state)) (error "variable not declared:" var))
      ((eq? (getMatch var state) "null") (error "variable not initialized" var))
      (else (getMatch var state)) )))

(define getMatch
  (lambda (var state)
    (cond
      ((null? (getVariables state)) (getMatch var (substate state)))
      ((eq? var (car (getVariables state))) (car (getValues state)))
      (else (getMatch var (constructSubstate (cdr (getVariables state)) (cdr (getValues state))  state))) )))  

(define declared?
  (lambda (var state)
    (cond
      ((include? var (getVariables state)) #t)
      ((hasSubstate state) (declared? (substate state)))
      (else #f) )))

(define declareVariable
  (lambda (var value state)
    (cond
      ((declared? var state) (error "redifining:" var))
      (else (addVariable var value state)))))

(define assignVariable
  (lambda (var value state)
    (cond
     ((not (declared? var state)) (error "variable not declared:" var))
      (else (addVariable var value (removeVariable var state)))))) 

(define addVariable
  (lambda (var value state)
    (constructState (list (cons var (getVariables state)) (cons value (getValues state))) state))) 
    
(define removeVariable
  (lambda (var state)
    (constructState (removeMatch var (getVariables state) (getValues state)) state)))

(define constructState
  (lambda (varLists state)
    (append (list (car varLists) (cadr varLists)) (cdr (cdr state)))))

(define constructSubstate
  (lambda (variables values subState)
    (append (list variables values subState))))


;utility functions------------------------------------------------------------------
(define include?
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((eq? x (car l)) #t)
      (else (include? x (cdr l))))))

(define removeLast
  (lambda (l)
    (cond
      ((eq? (length l) 1) '())
      (else (cons (car l) (removeLast (cdr l)))))))
    

(define matchingValue
  (lambda (x l1 l2)
    (cond
      ((null? l1) #f)
      ((eq? x (car l1)) (car l2))
      (else (matchingValue x (cdr l1) (cdr l2))))))

(define removeMatch
  (lambda (x l1 l2)
    (cond
      ((null? l1) '(() ()))
      ((eq? x (car l1)) (list (cdr l1) (cdr l2))) 
      (else (list (cons (car l1) (car (removeMatch x (cdr l1) (cdr l2)))) (cons (car l2) (cadr (removeMatch x (cdr l1) (cdr l2)))))))))
         
