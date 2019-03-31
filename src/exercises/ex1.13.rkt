#lang racket
(require sicp)
(require racket/trace)

(display "_____________________________\n")
(display "Prove Fib(n) is the closest integer to phi^n//sqrt(5) \n");

; Induction: f(0), f(n), f(n+1) -> proven for all n natural numbers.


; __________________________
; FIBONACCI
(define (fib n)
  (fib-iter 1 0 n)
  )

(define (fib-iter a b count)
  (if (= 0 count) b
      (fib-iter (+ a b) a (- count 1))
      )
  )


; __________________________
; phi function

(define phi
  (/ (+ 1 (sqrt 5) )
     2
     )
  )

(define (pow x n)
  (define (pow-inner n count)
    (if (= count n) 1
        (* x (pow-inner n (+ count 1)))
        )
    )
  (pow-inner n 0)
  )

(define (phi-fun n)
  (/ (pow phi n)
     (sqrt 5)
     )
  )

(define gamma
  (/ (- 1 (sqrt 5))
     2)
  )

(define (fib-via-phi n)
  (/ (- (pow phi n) (pow gamma n) )
     (sqrt 5))
  )

;___________________________
; test


(define (test-fun name func n)
  (display (format "~a(~a) = ~a\n" name n (func n)))
  )

(define (fib-test n)
  (test-fun "fib" fib n)
  (test-fun "fib-via-phi" fib-via-phi n)
  )

; __________________________
;(trace fun)

; INDUCTION

; n = 0
(fib-test 0)

; n = 1
(fib-test 1)
; n = 2
(fib-test 2)
(fib-test 3)
(fib-test 4)
(fib-test 5)
(fib-test 6)