#lang sicp

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (square x)
  (* x x))

(define (f a) (sum-of-squares (+ a 1) (* a 2)))

;; Expanding with applicative-order evaluation:
;; Reduce all arguments as far as possible,
;; Only then expand applications
(f 5)

(sum-of-squares (+ 5 1) (* 5 2))

(sum-of-squares 6 10)

(+ (square 6) (square 10))

(+ (* 6 6) (* 10 10))

(+ 36 100)

136

;; Expanding with normal-order evaluation:
;; First expand all functions until primitives remain,
;; Only then reduce.
(f 5)

(sum-of-squares (+ 5 1) (* 5 2))

(+ (square (+ 5 1)) (square (* 5 2)))
(+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
(+ 36 100)
136

;; Using `cond`
;; Paredit brackets getting in the way :|
(define (abs x)
  (cond ((> x 0) x)
                 ((= x 0) 0)
                 ((< x 0) (- x))))

(abs -123)
(abs 12)

;; Exercise 1.1

;; Result in comments

10 ;; 10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ (* 2 4) (- 4 6)) ;; 6
(define a 3) ;; 3 ? -- was nothing
(define b (+ a 1)) ;; nothing
(+ a b (* a b)) ;; 19
(= a b) ;; nil (elisp :') )
(if (and (> b a) (< b (* a b)))
    b
    a) ;; b -- Should have been 4 :)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;; 16
(+ 2 (if (> b a) b a)) ;; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;; 16

;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;; 1.3
(define (sum-of-largest-squares x y z)
  (sum-of-squares (max-of-three x y z) (mid-of-three x y z)))

(define (max-of-three x y z)
  (max (max x y) z))

(define (mid-of-three x y z)
  (if (middle-is-middle x y z)
      y
      (mid-of-three y z x)))

(define (middle-is-middle x y z)
  (or
   (is-sorted x y z)
   (is-sorted z y x)))

(define (is-sorted x y z)
  (and (<= x y) (<= y z)))

;; Tests
(and
 (= 13 (sum-of-largest-squares 1 2 3))
 (= 13 (sum-of-largest-squares 1 3 2))
 (= 13 (sum-of-largest-squares 2 3 1))
 (= 13 (sum-of-largest-squares 2 1 3))
 (= 13 (sum-of-largest-squares 3 2 1))
 (= 13 (sum-of-largest-squares 3 1 2))
 (= 8 (sum-of-largest-squares 1 2 2))
 (= 8 (sum-of-largest-squares 2 1 2))
 (= 8 (sum-of-largest-squares 2 2 1))
)

;; 1.4
;; It chooses (+ a b) or (- a b) depending on whether b is positive or negative.

;; 1.5
;; Normal order will recurse indefinitely

;; 1.6
;; It will always evaluate predicate, then-clause and else-clause,
;; which will lead to infinite recursion in this case -- the stop condition
;; is never tested.

;; 1.7
(define (avg x y) (/ (+ x y) 2))

(define (newton n)
  (newton-int n 1.0 0 0.0000001))

(define (newton-int n guess previous delta)
  (if (good-enough? guess previous delta)
      guess
      (newton-int n (improve guess n) guess delta)))

(define (good-enough? guess previous delta)
  (< (abs (- guess previous)) delta))

(define (improve guess n)
  (improve-for-cubed guess n)
)

(define (improve-for-cubed guess n)
  (/
   (+
    (/ n (square guess))
    (* 2 guess))
   3))

(define (improve-for-squared guess n)
  (avg guess (/ n guess)))

;; Nesting all the functions
(define (newton-rewritten n)
  (define (newton-int guess previous delta)
    (define (difference)
      (abs (- guess previous)))

    (define (good-enough?)
        (< (difference) delta))

    (define (improve)
      (cube-root-approximation))
    (define (cube-root-approximation)
      (/
       (+
        (/ n (square guess))
        (* 2 guess))
       3)
      )

    (define (square-root-approximation)
      (avg guess (/ n guess)))

    (if (good-enough?)
        guess
        (newton-int (improve) guess delta)))

  (newton-int 1.0 0 0.0000001))

(newton-rewritten 1)
(newton-rewritten 2)
(newton-rewritten 4)
(newton-rewritten 9)

[define (check-cube-root root)
  (newton-rewritten (* root root root))]

(check-cube-root 0.1)
(check-cube-root 0.01)
(check-cube-root 0.001)
(check-cube-root 0.0001)
