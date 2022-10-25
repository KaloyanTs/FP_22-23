#lang racket

(define (accumulate op nv a b term next)
  (if( > a b) nv
     (op (term a)
         (accumulate op nv (next a) b term next)
     )
  )
  )

(define (done? n)
  (= (+ n 2) (accumulate +
                         0
                         1
                         (quotient n 2)
                         (lambda (i)
                           (if
                            (zero? (remainder n i))
                            i
                            0
                            )
                           )
                         (lambda (i) (+ i 1))
                         )
     )
  )

(define (sum-almost-done a b)
  (define (min a b) (if (< a b) a b))
  (define (isGood? n)
    (define (dist-to-done n)
      (cond ((done? n) 0)
            ((or (< n a) (> n b)) (- (+ b 1) a))
            (else (min
                   (dist-to-done (+ n 1))
                   (dist-to-done (- n 1))
                   ))
            )
      )
    (define (is-first-least? a b c)
      (and (< a b) (< a c))
      )
    (is-first-least? (dist-to-done n) (abs (- n a)) (abs (- n b)))
    )
  (accumulate +
              0
              a
              b
              (lambda (i) (if (isGood? i) i 0))
              (lambda (i) (+ i 1))
              )
  )