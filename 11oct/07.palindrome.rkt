#lang racket
(require rackunit)
(require rackunit/text-ui)

;### зад 7
; Дали числото е палиндром?
(define (palindrome? n)
(define (reverse-digits n)
  (define (digits n)
    (if (< n 10)
        1
        (+ 1 (digits (quotient n 10)))
        )
    )
  (cond ((negative? n) (- 0 (reverse-digits (- 0 n))))
        ((< n 10) n)
        (else (+ (* (remainder n 10) (expt 10 (- (digits n) 1))) (reverse-digits (quotient n 10))))
        )
  )
  (= n (reverse-digits n))
  )

(run-tests
 (test-suite
  "palindrome? tests"
  (check-false (palindrome? 1234))
  (check-false (palindrome? 1231))
  (check-true (palindrome? 9102019))
  (check-true (palindrome? 10000001))
  (check-true (palindrome? 0))
  (check-true (palindrome? 5)))
 'verbose)

