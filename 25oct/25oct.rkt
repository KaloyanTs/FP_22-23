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
    (define (dist-to-done n dir)
      (cond ((done? n) 0)
            ((or (< n a) (> n b)) (- (+ b 1) a))
            (else
             (cond
               ((zero? dir)
                (min
                 (+ 1 (dist-to-done (+ n 1 ) 1))
                 (+ 1 (dist-to-done (- n 1) -1))
                 ))
               ((> dir 0) (+ 1 (dist-to-done (+ n 1) 1)))
               (else (+ 1 (dist-to-done (- n 1) -1)))
               )
             )
            )
      )
    (define (is-first-least? a b c)
      (and (< a b) (< a c))
      )
    (is-first-least? (dist-to-done n 0) (abs (- n a)) (abs (- n b)))
    )
  (accumulate +
              0
              a
              b
              (lambda (i) (if (isGood? i) i 0))
              (lambda (i) (+ i 1))
              )
  )

(define (product-digits n)
  (if (< n 10) n (* (remainder n 10) (product-digits (quotient n 10)))))

(define (curly n)
  (- n (product-digits n))
  )

(define (largest-diff a b)
  (define (diff-fixed n a b)
    (if (> a b) 0
        (max (abs (- (curly n) (curly a)))
             (diff-fixed n (+ a 1) b))
        )
    )
  (accumulate max
              0
              a
              b
              (lambda (i) (diff-fixed i (+ i 1) b))
              (lambda (i) (+ i 1))
              )
  )