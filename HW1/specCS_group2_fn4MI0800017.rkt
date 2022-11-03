#lang racket

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (mod7 x) (modulo x 7))
(define id (lambda (x) x))
(define ++ (lambda (i) (+ i 1)))

;(define pi 3.14159265359)

(define pi (* 4 (atan 1)))

(define (argmin f a b)
  (accumulate-i (lambda (x y) (if (< (f x) (f y))
                                  x
                                  y
                                  )
                  )
                (f a)
                a
                b
                id
                ++
                )
  )

(define (divisors n)
  (accumulate-i (lambda (x y) (if (zero? (remainder n y))
                                  (++ x)
                                  x
                                  )
                  )
                0
                1
                n
                id
                ++
                )
  )

(define (best-pair a b)
  (define cmp-pairs
    (lambda (x y) (if (> (divisors (+ (car x)
                                      (cdr x)
                                      )
                                   )
                         (divisors (+ (car y)
                                      (cdr y)
                                      )
                                   )
                         )
                      x
                      y
                      )
      )
    )
  (accumulate-i cmp-pairs
              (cons a (++ a))
              a
              (- b 1)
              (lambda (i) (accumulate-i cmp-pairs
                                      (cons i (++ i))
                                      (++ i)
                                      b
                                      (lambda (j) (cons i j))
                                      ++
                                      )
                )
              ++
              )
  )

(define (integrate2 f a b c d dx dy)
  #t
  )