#lang racket

(define (tail n)
  (if (< n 10) 0
      (+
       (* 10 (tail (quotient n 10)))
       (remainder n 10)
       )
      )
  )

(define (head n)
  (if (< n 10)
      n
      (head (quotient n 10))
      )
  )

(define (rot-left n)
  (+ (head n) (* 10 (tail n)))
  )

(define (dig-count n)
  (if (< n 10)
      1
      (+ 1 (dig-count (quotient n 10)))
      )
  )

(define (fixed-last-to-second n k)
  (define (helper cur d)
    (if (< cur (expt 10 k))
        (+ (* 10 cur) d)
        (+
         (*
          10
          (helper
           (quotient cur 10)
           d
           )
          )
         (remainder cur 10)
         )
        )
    )
  (helper (quotient n 10) (remainder n 10))
  )

(define (fixed n k)
  (define (iter res i m)
    (if (> i m)
        res
        (iter (fixed-last-to-second res k) (+ i 1) m)
        )
    )
  (if (zero? k)
      (rot-left n)
      (iter n 1 (- (dig-count n) 1 k))
      )
  )

(define (max-rot n)
  (let ((d (dig-count n)))
    (define (iter i cur max)
      (if (>= i d)
          max
          (iter (+ i 1)
                (fixed cur i)
                (if (> cur max) cur max)
                )
          )
      )
    (iter 0 n n)
    )
  )