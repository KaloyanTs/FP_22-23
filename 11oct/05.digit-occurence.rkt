#lang racket
(require rackunit)
(require rackunit/text-ui)
; зад 5
; колко пъти цифрата d се среща в цялото число n?
(define (digit-occurence d n)
  (cond ((< n 0) (digit-occurence d (- 0 n)))
        ((< n 10) (if (= n d) 1 0))
        ((= (remainder n 10) d) (+ 1 (digit-occurence d (quotient n 10))))
        (else (digit-occurence d (quotient n 10)))
        )
  )

(run-tests
 (test-suite
  "digit-occurence tests"
  (check = (digit-occurence 2 12305) 1)
  (check = (digit-occurence 0 10000) 4)
  (check = (digit-occurence 0 -1093) 1)
  (check = (digit-occurence 2 10000001) 0))
 'verbose)