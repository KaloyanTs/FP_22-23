#lang racket
(require rackunit)
(require rackunit/text-ui)

;### зад 7
; Сбор от делителите на число.
(define (sum-divisors n)
  (define (iter i s)
    (cond ((> i n) s)
          ((= (remainder n i) 0) (iter (+ i 1) (+ s i)))
          (else (iter (+ i 1) s))
          )
   )
  (iter 1 0)
  )

(run-tests
  (test-suite
    "sum-divisors tests"
    (check = (sum-divisors 3) 4) ; 1 3
    (check = (sum-divisors 12) 28) ; 1 2 3 4 6 12
    (check = (sum-divisors 15) 24) ; 1 3 5 15
    (check = (sum-divisors 19) 20) ; 1 19
    (check = (sum-divisors 42) 96) ; 1 2 3 6 7 14 21 42
    (check = (sum-divisors 661) 662) ; 1 661
    (check = (sum-divisors 666) 1482) ; 1 2 3 6 9 18 37 74 111 222 333 666
    (check = (sum-divisors 1337) 1536) ; 1 7 191 1337
    (check = (sum-divisors 65515) 78624) ; 1 5 13103 65515
    (check = (sum-divisors 1234567) 1244416)) ; 1 127 9721 1234567
  'verbose)
