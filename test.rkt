(load "interpret.rkt")
(require racket/trace)

(define expectedValues
  '(150 -4 10 16 220 5 6 10 5 -39 true 100 false true 128 12))

(define runTest
  (lambda (i values)
    (cond
      ((> i 16) #t)
      ((not (list? values)) "Why")
      ((eq? (interpret (string-append "tests/test" (string-append (number->string i) ".txt"))) (car values))
       (runTest (+ i 1) (cdr values)))
      (else (print (car values)) #f))))

(trace runTest)
(trace getMatch)

(define runTests
  (runTest 1 expectedValues))


       
    
    
 

      
      