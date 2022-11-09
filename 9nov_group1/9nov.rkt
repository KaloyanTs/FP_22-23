#lang r5rs

(define (filter p? lst)
  (cond ((null? lst) '())
        ((p? (car lst)) (cons (car lst)
                              (filter p? (cdr lst))
                              )
                        )
        (else (filter p? (cdr lst)))
        )
  )

(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l)
          (foldr op
                 nv
                 (cdr l)
                 )
          )
      )
  )

(define (count el lst)
  (length (filter (lambda (x) (= el x))
                  lst
                  )
          )
  )

(define (only-uniques l)
  (filter (lambda (x) (< (count x l)
                         2
                         )
            )
          l
          )
  )

(define (max-unique-single l)
  (define u (only-uniques l))
  (if (null? u)
      #f
      (foldr (lambda (x y) (if (not y)
                               x
                               (max x y)
                               )
               )
             (car u)
             (cdr u)
             )
      )
  )

(define (max-unique ll)
  (foldr (lambda (x y) (cond ((not x) y)
                             ((not y) x)
                             (else (max x y))
                             )
           )
         #f
         (map max-unique-single ll)
         )
  )