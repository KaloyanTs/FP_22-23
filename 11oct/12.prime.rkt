#lang racket
(require rackunit)
(require rackunit/text-ui)

;### зад 12
; Дали числото е просто?
; > Дадено цяло число n е просто, ако не се дели на никое от числата между 2 и n-1 (даже [от 2 до √n][primality-test]).
(define (prime? n)
  (define (any-divisor i n r)
    (if (> i (- n 1))
        r
        (any-divisor (+ i 1) n (or r (= (remainder n i) 0)))
        )
    )
  (and (> n 1) (not (any-divisor 2 n #f)))
  )

(run-tests
  (test-suite
    "prime? tests"
    (check-false (prime? 0))
    (check-false (prime? 1))
    (check-false (prime? -120))
    (check-false (prime? 120))
    (check-true (prime? 2))
    (check-true (prime? 3))
    (check-true (prime? 7))
    (check-true (prime? 101))
    (check-true (prime? 2411)))
  'verbose)
