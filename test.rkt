(load "interpret.rkt")
(require racket/trace)

(define expectedValues1
  '(150 -4 10 16 220 5 6 10 5 -39 true 10 false true 128 12)
  )

(define expectedValues2
  '(20 164 32 2 25 21 6 -1 789 12 125 110 2000400 101)
  )

(define interpretTest1
  (lambda (i)
    (interpret (string-append "tests/test1" (string-append (number->string i) ".txt")))
    ))

(define runTest1
  (lambda (i values)
    (cond
      ((> i 16) #t)
      ((not (list? values)) "Why")
      ((eq? (interpretTest i) (car values)) (runTest (+ i 1) (cdr values)))
      (else (error "Test" i "should be equal to" (car values) "not" (interpretTest i)))
      )))

(define interpretTest2
  (lambda (i)
    (interpret (string-append "tests/test2" (string-append (number->string i) ".txt")))
    ))

(define runTest2
  (lambda (i values)
    (cond
      ((> i 16) #t)
      ((not (list? values)) "Why")
      ((eq? (interpretTest i) (car values)) (runTest (+ i 1) (cdr values)))
      (else (error "Test" i "should be equal to" (car values) "not" (interpretTest i)))
      )))

(define runTests
  (print (runTest1 1 expectedValues1))
  (print (runTest 2 expectedValues2))
  )     
