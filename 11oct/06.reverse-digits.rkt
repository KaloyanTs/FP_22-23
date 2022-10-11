#lang racket
(require rackunit)
(require rackunit/text-ui)

;### зад 6
; Обръща реда на цифрите на число.

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
  
(run-tests
 (test-suite
  "reverse-digits tests"
  (check = (reverse-digits 12305) 50321)
  (check = (reverse-digits 10000) 1)
  (check = (reverse-digits -1093) -3901)
  (check = (reverse-digits 10000001) 10000001))
 'verbose)

