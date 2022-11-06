#lang r5rs

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

(define (integrate f a b dx)
  (* dx (accumulate-i +
                      0
                      a
                      b
                      f
                      (lambda (i) (+ i dx))
                      )
     )
  )

(define (integrate2 f a b c d dx dy)
  (* dy (accumulate-i +
                      0
                      c
                      d
                      (lambda (y) (integrate (lambda (x) (f x y))
                                             a
                                             b
                                             dx
                                             )
                        )
                      (lambda (y) (+ y dy))
                      )
     )
  )

(define (n-rooks board n)
  (and (accumulate-i (lambda (x y) (and x y))
                     #t
                     0
                     (- n 1)
                     (lambda (i) (accumulate-i (lambda (x y) (or x y))
                                               #f
                                               0
                                               (- n 1)
                                               (lambda (j) (board j i n))
                                               ++
                                               )
                       )
                     ++
                     )
       (accumulate-i (lambda (x y) (and x y))
                     #t
                     0
                     (- n 1)
                     (lambda (i) (accumulate-i (lambda (x y) (or x y))
                                               #f
                                               0
                                               (- n 1)
                                               (lambda (j) (board i j n))
                                               ++
                                               )
                       )
                     ++
                     )
       )
  )

(define (take l i)
  (car (accumulate-i (lambda (x y) (cdr x))
                     l
                     1
                     (- i 1)
                     id
                     ++
                     )
       )
  )

(define (acc-foldr op nv l)
  (accumulate op
              nv
              1
              (length l)
              (lambda (i) (take l i))
              ++
              )
  )

(define (n-rooks2 board n)
  (and (accumulate (lambda (x y) (and x y))
                   #t
                   0
                   (- n 1)
                   (lambda (i) (acc-foldr (lambda (x y) (or (= (car x) i) y))
                                      #f
                                      board
                                      )
                     )
                   ++
                   )
       (accumulate (lambda (x y) (and x y))
                   #t
                   0
                   (- n 1)
                   (lambda (i) (acc-foldr (lambda (x y) (or (= (cdr x) i) y))
                                      #f
                                      board
                                      )
                     )
                   ++
                   )
       )
  )