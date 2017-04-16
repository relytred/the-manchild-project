#|
Jeffrey Noymer & Tyler Mayle
jan88 & tcm45
Project 1
|#

(require racket/trace)
;state functions---------------------------------------------------------------
(define newstate '(() () ()))
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

;gets param names of a function
(define getParamNames
  (lambda (function)
    (cadr function)))

; A function to get the functions of a given state
(define getFunctions
  (lambda (state)
    (caddr state)))

(define hasFunc?
  (lambda (state)
    (> (length (getFunctions state)) 0)))

; A function to get the substate of given state

(define substate
  (lambda (state)
    (cadddr state)))

; A function to check if a state has a substate

(define hasSubstate
  (lambda (state)
    (eq? (length state) 4) ))

; A function to add a substate to a given state

(define addSubstate
  (lambda (state)
    (cond
      ((hasSubstate state) (constructSubstate (getVariables state) (getValues state) (getFunctions state) (addSubstate (substate state))))
      (else (constructSubstate (getVariables state) (getValues state) (getFunctions state) newstate)) )))

; A function to remove a substate from the given state

(define removeSubstate
  (lambda (state)
    (cond
      ((hasSubstate (substate state)) (constructSubstate (getVariables state) (getValues state) (getFunctions state) (removeSubstate (substate state))))
      (else (removeLast state)) )))

; A function to get the value of a given state

(define getValue
  (lambda (var state)
    (cond
      ((not (declared? var state)) (error "variable not in scope" var))
      ((eq? (unbox (first (getMatches var state))) "null") (error "variable not initialized" var))
      (else (unbox (first (getMatches var state)))) )))

; A function to find the matching value for the variable in a given state

(define getMatches
  (lambda (var state)
    (cond
      ((and (null? (getVariables state)) (hasSubstate state)) (getMatches var (substate state)))
      ((null? (getVariables state)) '())
      ((eq? var (first (getVariables state)))
       (append (getMatches var (constructState (pop (getVariables state)) (pop (getValues state))  state)) (cons (first (getValues state)) '())))
      (else (getMatches var (constructState (pop (getVariables state)) (pop (getValues state))  state))) )))  

; A fuction to determine weather or not a variable has been declared yet

(define declared?
  (lambda (var state)
    (cond
      ((include? var (getVariables state)) #t)
      ((hasSubstate state) (declared? var (substate state)))
      (else #f) )))

(define varOverwrite?
  (lambda (var state)
    (cond
      ((and (hasSubstate state) (hasFunc? state))  (varOverwrite? var (substate state)))
      ((include? var (getVariables state)) #t)
      ((hasSubstate state) (varOverwrite? var (substate state)))
      (else #f) )))

; A function to declare a variable and add it to a state

(define declareVariable
  (lambda (var value state)
    (cond
      ((varOverwrite? var state) (error "redifining:" var))
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
    (begin (set-box! (first (getMatches var state)) value) state)))

; A function to add a variable to a state

(define addVariable
  (lambda (var value state)
    (cond
      ((hasSubstate state) (constructSubstate (getVariables state) (getValues state) (getFunctions state) (addVariable var value (substate state))))
      (else (constructState (push var (getVariables state)) (push (box value) (getValues state)) state)) )))

;a function to create a function

(define defineFunction
  (lambda (name params body state)
    (cond
      ((defined? name state) (error "function already defined" name))
      (else (addFunction name params body state)) )))

;a list includes a certain function
(define includeFunc?
 (lambda (name l)
   (cond
     ((null? l) #f)
     ((eq? name (car (car l))) #t)
     (else (includeFunc? name (cdr l))) )))

;is a function defined?

(define defined?
  (lambda (name state)
    (cond
      ((includeFunc? name (getFunctions state)) #t)
      ((hasSubstate state) (defined? name (substate state)))
      (else #f) )))
    
;add function to state
(define addFunction
  (lambda (name params body state)
    (cond
      ((hasSubstate state) (constructSubstate (getVariables state) (getValues state) (getFunctions state) (addFunction name params body (substate state))))
      (else (constructStateFunction (cons (list name params body ) (getFunctions state)) state)) )))

(define getFunc
  (lambda (name l)
    (cond
      ((eq? name (car (car l))) (car l))
      (else (getFunc name (cdr l))) )))

(define getFunction
  (lambda (name state)
    (cond
      ((includeFunc? name (getFunctions state)) (getFunc name (getFunctions state)) )
      (else (getFunction name (substate state))) )))

(define addParams
  (lambda (paramNames paramValues state)
    (cond
      ((not (eq? (length paramNames) (length paramValues))) (error "Mismatched parameters and arguments, Expected:" (length paramNames) "not" (length paramValues)))
      ((null? paramNames) state)
      (else (addParams (cdr paramNames) (cdr paramValues) (declareVariable (car paramNames) (car paramValues) state))) )))

(define getFunctionBody
  (lambda (name state)
    (caddr (getFunction name state)) ))

(define getFunctionParams
  (lambda (name state)
    (cadr (getFunction name state)) ))

(define replaceSubstate
  (lambda (state newSubstate)
    (list (getVariables state) (getValues state) (getFunctions state) newSubstate)))
          
(define removeFunctionState
  (lambda (name state)
    (cond
      ((includeFunc? name (getFunctions state)) (replaceSubstate state newstate))
      (else (replaceSubstate state (removeFunctionState name (substate state)))) )))   
  
(define createFunctionState
  (lambda (functionName params state)
    (addParams (getFunctionParams functionName state) params (addSubstate (removeFunctionState functionName state))) ))
    
; A function to construct a state

(define constructState
  (lambda (vars values state)
    (append (list vars values) (cddr state))))

; A function to construct a substate

(define constructSubstate
  (lambda (variables values functions subState)
    (append (list variables values functions subState))))

(define constructStateFunction
  (lambda (functions state)
    (append (list (getVariables state) (getValues state) functions) (cdddr state))))


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

(define replaceMatch
  (lambda (x l1 l2 value)
    (cond
      ((null? l1) #f)
      ((eq? x (first l1)) (set-box! (first l2) value)) 
      (else (replaceMatch x (pop l1) (pop l2) value)) )))
         
