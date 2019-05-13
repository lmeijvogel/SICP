#lang sicp

;; Factorials

;; Recursive
(define (factorial-rec n)
  (cond ((= n 1) 1)
        ((> n 1) (* n (factorial-rec (- n 1))))))

;; Running total

(define (factorial-running n)
  (define (factorial-iter curr n total)
    (if (> curr n)
        total
        (factorial-iter (+ 1 curr) n (* curr total)))
    )

  (factorial-iter 1 n 1))

(define (factorial-reverse n)
  (define (factorial-reverse-iter curr total)
    (if (= curr 0) total
        (factorial-reverse-iter (- curr 1) (* curr total))))

  (factorial-reverse-iter n 1))

(define (factorial n)
  ;; (factorial-rec n)
  ;; (factorial-running n)
  (factorial-reverse n)
)

(factorial 1)
(factorial 3)
(factorial 5)

;; 1.9

(define (++ a b)
  (if (= a 0)
      b
      (inc (++(dec a) b))))

;; expand:

;; (++ 4 5)
;; (inc (++ 3 5))
;; (inc (inc (++ 2 5)))
;; (inc (inc (inc (++ 1 5))))
;; (inc (inc (inc (inc (++ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9

;; 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 0 10)
(A 1 10)
(A 1 4)
(A 2 4)
(A 3 3)


;; Re 'give concise definitions': Is this just a rewording of the function definition?

(define (f n) (A 0 n)) ;; (f n) = (* 2 n)
(define (g n) (A 1 n)) ;; (g n) = (A 0 (A 1 (- n 1)))
                       ;;       = (* 2 (A 1 (- n 1)))
                       ;;       = (** 2 n) exp

(define (h n) (A 2 n)) ;; (h n) = (A 1 (A 2 (- n 1)))
                       ;;       = (g (A 2 (- n 1)))
                       ;;       = (** 2 (h (- n 1)))

;; (g 3) = (* 2 (A 1 2))
;;       = (* 2 (* 2 (A 1 1)))
;;       = (* 2 (* 2 2))
;;       = (* 2 4)
;;       = 8

;; Fibonacci iteratively

(define (fib n)
  (fib-iter 0 n 0 1))

(define (fib-iter s n prev curr)
                  (if (= s n)
                      curr
                      (fib-iter (+ s 1) n curr (+ curr prev))))


(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)

;; 1.11

;; f: f(n) = n if n < 3
;;    f(n) = f(n-1) + 2f(n - 2) + 3f(n - 3) otherwise

(display "\n\n1.11:\n")

(define (f11-rec n)
  (cond ((< n 3) n)
        (else
         (+ (+
              (f11-rec (- n 1))
              (* 2 (f11-rec (- n 2))))
            (* 3 (f11-rec (- n 3)))))
        ))

(define (f-print f n)
  (display "  ")
  (display n)
  (display ": ")
  (display (f n))
  (display "\n"))

(f-print f11-rec 1)
(f-print f11-rec 2)
(f-print f11-rec 3)
(f-print f11-rec 4)
(f-print f11-rec 5)
(f-print f11-rec 6)
(f-print f11-rec 7)
(f-print f11-rec 8)
(f-print f11-rec 9)

(define (f11-iter n)
  (define (calc-new current n1 n2)
      (+ (+ current
            (* 2 n1))
            (* 3 n2)))

  (define (f11-iter-int i fn fn1 fn2)
    (cond ((= n i) fn)
          (else (f11-iter-int
                 (+ i 1)
                 (calc-new fn fn1 fn2)
                 fn
                 fn1)))

    )

  (cond ((< n 3) n)
        (else (f11-iter-int 2 2 1 0)))
  )

(display "Recursive:\n")
(f-print f11-iter 1)
(f-print f11-iter 2)
(f-print f11-iter 3)
(f-print f11-iter 4)
(f-print f11-iter 5)
(f-print f11-iter 6)
(f-print f11-iter 7)
(f-print f11-iter 8)
(f-print f11-iter 9)

(display "Pascal\n")

(define (pascal y x)
  (cond ((= x 0) 1)
        ((= y 0) 1)
        ((= x y) 1)
        (else (+
               (pascal (- y 1) (- x 1))
               (pascal (- y 1) x)
               )
              )
        ))

(define (print-pascal-row y)
  (define (print-row-part y x)
    (display (pascal y x))
    (display " ")

    (if (= x y)
        (display "\n")
        (print-row-part y (+ x 1))
        ))

  (print-row-part y 0))

(define (print-pascal height)
  (define (print-pascal-iter height curr)
    (print-pascal-row curr)

    (if (< curr height)
        (print-pascal-iter height (+ curr 1))
    )
  )

  (print-pascal-iter height 0)
  )

(print-pascal 10)

(display "1.13\n")

(define (phi)
  (/ (+ (sqrt 5) 2)))

(define (psi)
  (/ (- (sqrt 5) 2)))

(fib 1)

; Exponentiation
(display "---- Exponentiation")

(define (expt b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b (- counter 1) (* product b)))
  )
  (expt-iter b n 1))

(display "---- expt")

(expt 2 0)
(expt 2 2)
(expt 2 4)
(expt 2 6)
(expt 3 4)

(define (square n) (* n n))
(define (fast-expt b n)
  (define (even? n) (= 0 (remainder n 2)))

  (cond ((= n 0) 1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))

(display "---- fast-expt")
(fast-expt 2 0)
(fast-expt 2 2)
(fast-expt 2 3)
(fast-expt 2 5)
(fast-expt 3 4)

(display "---- other-fast-expt")

(define (other-fast-expt b n)
  (define (other-fast-expt-iter b n a)
    (cond ((= n 0) 1)
          ((= n 1) a)
          ((even? n) (other-fast-expt-iter b (/ n 2) (* a (square b))))
          (else (other-fast-expt-iter b (- n 1) (* a b)))

    )
  )
  (other-fast-expt-iter b n 1)
)

(other-fast-expt 2 0)
(other-fast-expt 2 2)
(other-fast-expt 2 3)
(other-fast-expt 2 5)
(other-fast-expt 3 4)

(display "---- Multiplication")

(display "---- mult")
(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))
      ))

(mult 2 0)
(mult 2 2)
(mult 2 3)
(mult 2 5)
(mult 3 4)

(define (fast-mult a b)
  (define (doub n) (+ n n))

  (define (even? n) (= 0 (remainder n 2)))

  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b)
         (doub (fast-mult a (/ b 2))))
        (else
         (* b (fast-mult a (- b 1))))))

(display "---- fast-mult\n")
(fast-mult 2 0)
(fast-mult 2 2)
(fast-mult 2 3)
(fast-mult 2 5)
(fast-mult 3 4)

(display "---- gdc\n")

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1))))
)

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 2)
(prime? 8)
(prime? 13)
(prime? 27)
(prime? 29)

(display "---- gdc - Fermat\n")
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))

  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
          ((fermat-test n)
            (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 2 10)
(fast-prime? 8 10)
(fast-prime? 13 10)
(fast-prime? 27 10)
(fast-prime? 29 10)

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

(define (time-printed-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-time (- (runtime) start-time))))

(define (report-time elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes number min)
  (define (report-and-continue number n start-time)
    (display n)
    (display " *** ")
    (display (- (runtime) start-time))
    (display "\n")
    (search-for-primes (- number 1) (+ n 2)))

  (define (search-for-primes-iter number n start-time)
    (if (> number 0)
        (if (prime? n)
            (report-and-continue number n start-time)
            (search-for-primes-iter number (+ n 2) start-time)
            )))

  (if (even? min)
    (search-for-primes-iter number (+ min 1) (runtime))
    (search-for-primes-iter number min (runtime))))

(search-for-primes 8 1000000)
