(load "interpret.rkt")
(require racket/trace)

(define expectedValues
  '(150 -4 10 16 220 5 6 10 5 -39 true 100 false true 128 12))

(define interpretTest
  (lambda (i)
    (interpret (string-append "tests/test" (string-append (number->string i) ".txt"))) ))

(define runTest
  (lambda (i values)
    (cond
      ((> i 16) #t)
      ((not (list? values)) "Why")
      ((eq? (interpretTest i) (car values)) (runTest (+ i 1) (cdr values)))
      (else (error "Test" i "should be equal to" (car values) "not" (interpretTest i)))   )))

(define runTests
  (print (runTest 1 expectedValues)))


       
    
    
 

      
      