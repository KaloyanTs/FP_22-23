#lang racket
(define (f2 x y)
  (define (in-box? x y)
    (and (<= (abs x) 1) (<= (abs y) 1))
    )
  (and (in-box? x y)
       (in-box? (- x 2) (- y 2))
       (in-box? (- x 4) (- y 4))
       )
  )

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))
        )
  )

(define (div-sum n)
  (define (iter i s)
    (if (<= i n)
        (if (= 0 (remainder n i))
            (iter (+ i 1) (+ s i))
            (iter (+ i 1) s)
            ) 
        s
        )
    )
  (iter 1 0)
  )

(define (perfect? n)
  (= (* 2 n) (div-sum n))
  )

(define (prime? n)
  (define (any-div i)
    (if (> i (quotient n 2))
        #f
        (if (= 0 (remainder n i))
            #t
            (any-div (+ i 1))
            )
        )
    )
  (not (any-div 2))
  )

(define (increasing? n)
  (if (< n 10)
      #t
      (and (>
            (remainder n 10)
            (quotient (- (remainder n 100) (remainder n 10)) 10)
            )
           (increasing? (quotient n 10))
           )
      )
  )

(define (toBinary n)
  (cond ((= n 1) 1)
        ((even? n) (* 10 (toBinary (quotient n 2))))
        (else (+ 1 (* 10 (toBinary (quotient n 2)))))
        )
  )

(define (toDecimal n)
  (if (= n 1)
      1
      (+ (remainder n 10) (* 2 (toDecimal (quotient n 10))))
      )
  )