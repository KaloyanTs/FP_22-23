#lang racket
(require rackunit)
(require rackunit/text-ui)

;### зад 10
; Итеративен вариант на Exponentian by squaring (бързо умножение)
(define (fasterpow x n)
  (define (sq y) (* y y)) 
  (cond ((= n 0) 1)
        ((even? n) (sq (fasterpow x (quotient n 2))))
        (else (* x (fasterpow x (- n 1))))
        )
  )

(run-tests
  (test-suite
    "fasterpow tests"
    (check = (fasterpow 3 15) 14348907)
    (check = (fasterpow 10 4) 10000)
    (check = (fasterpow 5 3) 125)
    (check = (fasterpow -5 3) -125)
    (check = (fasterpow -5 0) 1))
  'verbose)

