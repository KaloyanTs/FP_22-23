#lang racket
(define x 5)
(+ 1/4 2/5 3/8 (* 6 (- 5.1 1.6) (- 9/3 7/4)))
(quotient (expt 3 (quotient 60 7)) (quotient (expt 2 10)179))
(expt 1-i 21)

(define (odd? n)
  (= (remainder n 2) 1)
  )

(define even?
  (compose not odd?)
  )

(define a 5)

(display "sad\n")

(define var (display "var se oceni\n"))
(define (f) (display "f se oceni\n"))

(define (grade points)
  (+ (/ points 40) 3/2)
  )

(define (grade2 points)
  (if (< points 60)
      '(2,slab)
      (if (< points 80)
          '(3,sreden)
          (if (< points 120)
              '(4,dobyr)
              (if (< points 160)
                  '(5,mn dobyr)
                  '(6,otlichen))
              )
          )
      )
  )

(define (grade3 points)
  (cond
    ((< points 60) 2)
    ((< points 80) 3)
    ((< points 120) 4)
    ((< points 160) 5)
    (else 6)))


