#lang racket

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))
  )
  )

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))
      )
  )

(define (sum-interval a b)
  (if (> a b)
      0
      (+ a (sum-interval (+ a 1) b))
      )
  )

(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))
      )
  )

(define (reverse-digits n)
  (define (merge a b)
    (+ (* a (expt 10 (count-digits b))) b)
    )
  (if (< n 10)
      n
      (merge (remainder n 10) (reverse-digits (quotient n 10)))
      )
  )

(define (rev n)
  (define (first n)
    (if (< n 10)
        n
        (first (quotient n 10))
        )
    )
  (define (rem-first n)
    (if (< n 10)
        0
        (+ (remainder n 10) (* 10 (rem-first (quotient n 10))))
        )
    )
  (if (< n 10)
      n
      (+ (first n) (* 10 (rem-first n)))
      )
  )

(define (palindrome? n)
  (= n (reverse-digits n))
  )