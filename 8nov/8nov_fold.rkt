#lang racket

;(intersection '(1 2 3 4) '(6 1 4 5)) -> '(1 4)

(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l) (foldr op nv (cdr l)))
      )
  )

(define (filter p? l)
  (cond ((null? l) '())
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))
        )
  )

(define (member? el l)
  (foldr (lambda (x y) (or y
                           (equal? el
                                   x
                                   )
                           )
           )
         #f
         l
         )
  )

(define (uniques l1)
  (cond ((null? l1) '())
        ((member? (car l1)
                  (cdr l1)
                  )
         (uniques (cdr l1))
         )
        (else (cons (car l1) (uniques (cdr l1))))
        )
  )

(define (intersection l1 l2)
  (define l (uniques l1))
  (filter (lambda (x) (member? x l2))
          l)
  )

(define (union l1 l2)
  (uniques (append l1 l2))
  )

(define (set-minus l1 l2)
  (define l (uniques l1))
  (filter (lambda (x) (not (member? x l2)))
          l)
  )

(define (min-el less l)
  (foldr (lambda (x y) (if (less x y)
                           x
                           y
                           )
           )
         (car l)
         (cdr l)
         )
  )

(define (remove el lst)
  (cdr (foldr (lambda (y x) (if (and (not (car x)) (eq? el y))
                                (cons #t (cdr x))
                                (cons (car x) (cons y (cdr x)))
                                )
                )
              (list #f)
              lst
              )
       )
  )

(define (selection-sort less l)
  (if (< (length l) 2)
      l
      (let ((m (min-el less l)))
        (cons (min-el less l)
              (selection-sort less (remove m l))
              )
        )
      )
  )

(define (take n l)
  (if (zero? n)
      '()
      (cons (car l)
            (take (- n 1)
                  (cdr l)
                  )
            )
      )
  )

(define (slice from to l)
  (if (= from 1)
      (take to l)
      (slice (- from 1)
             (- to 1)
             (cdr l)
             )
      )
  )