#|
Jeffrey Noymer & Tyler Mayle
jan88 & tcm45
Project 4
|#

(load "interpret.rkt")
(require racket/trace)


; A set of expected values testing the first part of the project

(define expectedValues1
  '(150 -4 10 16 220 5 6 10 5 -39 true 100 false true 128 12)
  )

; A set of expected values testing the second part of the project

(define expectedValues2
  '(20 164 32 2 25 21 6 -1 789 12 125 110 2000400 101)
  )

; A set of expected values testing the third part of the project

(define expectedValues3
  '(10 14 45 55 1 115 true 20 24 2 35 90 69 87 64 125 100 2000400)
  )

; A set of expected values testing the fourth part of the project

(define expectedValues4
  '(15 12 125 36 54 110 26 117 32 15 123456 5285 -716)
  )

; A function to get the correct test.txt file for test suite 1

(define interpretTest1
  (lambda (i)
    (interpret (string-append "tests/test1/test" (string-append (number->string i) ".txt")))
    ))

; A master function to run the tests for test suite 1

(define runTest1
  (lambda (i values)
    (cond
      ((> i 16) #t)
      ((not (list? values)) "Why")
      ((eq? (interpretTest1 i) (car values)) (runTest1 (+ i 1) (cdr values)))
      (else (error "Test" i "should be equal to" (car values) "not" (interpretTest1 i)))
      )))

; A function to get the correct test.txt file for test suite 2

(define interpretTest2
  (lambda (i)
    (interpret (string-append "tests/test2/test" (string-append (number->string i) ".txt")))
    ))

; A master function to run the tests for test suite 2

(define runTest2
  (lambda (i values)
    (cond
      ((> i 14) #t)
      ((not (list? values)) "Why")
      ((eq? (interpretTest2 i) (car values)) (runTest2 (+ i 1) (cdr values)))
      (else (error "Test" i "should be equal to" (car values) "not" (interpretTest2 i)))
      )))

; A function to get the correct test.txt file for test suite 3

(define interpretTest3
  (lambda (i)
    (interpret (string-append "tests/test3/test" (string-append (number->string i) ".txt")))
    ))

; A master function to run the tests for test suite 3

(define runTest3
  (lambda (i values)
    (cond
      ((> i 18) #t)
      ((not (list? values)) "Why")
      ((eq? (interpretTest3 i) (car values)) (runTest3 (+ i 1) (cdr values)))
      (else (error "Test" i "should be equal to" (car values) "not" (interpretTest3 i)))
      )))

; A function to get the correct test.txt file for test suite 4

(define interpretTest4
  (lambda (i)
    (interpret (string-append "tests/test4/test" (string-append (number->string i) ".txt")))
    ))

; A master function to run the tests for test suite 4

(define runTest4
  (lambda (i values)
    (cond
      ((> i 13) #t)
      ((not (list? values)) "Why")
      ((eq? (interpretTest4 i) (car values)) (runTest4 (+ i 1) (cdr values)))
      (else (error "Test" i "should be equal to" (car values) "not" (interpretTest4 i)))
      )))

; A function to run all of the test suites

(define runTests
  (print (and ;(runTest1 1 expectedValues1)
              ;(runTest2 1 expectedValues2)
              ;(runTest3 1 expectedValues3)
              (runTest4 1 expectedValues4)
              )))