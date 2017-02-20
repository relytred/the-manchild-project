(load "simpleParser.scm")
;state functions---------------------------------------------------------------
(define getVariables
  (lambda (state)
    (car state)))

(define getValues
  (lambda (state)
    (cadr state)))

(define declared?
  (lambda (var state)
    (include? (getVariables state))))

(define declareVariable
  (lambda (var state)
    (addVariable var "null" state)))

(define addVariable
  (lambda (var value state)
    (constructState (list (cons var (getVariables state)) (cons value (getValues state))) state))) 
    

(define removeVariable
  (lambda (var state)
    (constructState (removeMatch var (getVariables state) (getValues state)) state)))

(define constructState
  (lambda (varLists state)
    (append (list (car varLists) (cadr varLists)) (cdr (cdr state)))))

(define cdrVariables
  (lambda (state)
    (constructState (cdr (getVariables state)) (cdr (getValues state)) state)))


;utility functions------------------------------------------------------------------
(define include?
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((eq? x (car l)) #t)
      (else (include? x (cdr l))))))

(define removeMatch
  (lambda (x l1 l2)
    (cond
      ((null? l1) '(() ()))
      ((eq? x (car l1)) (list (cdr l1) (cdr l2))) 
      (else (list (cons (car l1) (car (removeMatch x (cdr l1) (cdr l2)))) (cons (car l2) (cadr (removeMatch x (cdr l1) (cdr l2)))))))))
         


      


