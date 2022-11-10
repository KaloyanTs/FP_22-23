#lang racket

(define (all? p? l)
  (foldr (lambda (x y) (and (p? x) y))
         #t
         l
         )
  )

(define (zip a b)
  (if (or (null? a)
          (null? b)
          )
      '()
      (cons (cons (car a) (car b))
            (zip (cdr a) (cdr b))
            )
      )
  )

(define (is-maj a b)
  (and (<= (length a)
           (length b)
           )
       (all? (lambda (x) (<= (car x) (cdr x))) (zip a b))
       )
  )

(define (has-maj-sublist a b)
  (if (< (length b) (length a))
      #f
      (or (is-maj a b) (has-maj-sublist a (cdr b)))
      )
  )

(define (is-major? l)
  (if (< (length l)
         2
         )
      #t
      (and (has-maj-sublist (car l) (cadr l))
           (is-major? (cdr l))
           )
      )
  )

(define (take n l)
  (if (zero? n) '()
      (cons (car l) (take (- n 1) (cdr l)))
      )
  )

(define (from-to a b l)
  (if (= a 1) (take b l)
      (from-to (- a 1) (- b 1) (cdr l)) 
      )
  )

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next)
          )
      )
  )

(define (all-sublists l)
  (accumulate append
              '()
              1
              (length l)
              (lambda (i) (accumulate cons
                                      '()
                                      i
                                      (length l)
                                      (lambda (j) (from-to i j l))
                                      (lambda (j) (+ j 1))
                                      )
                )
              (lambda (i) (+ i 1))
              )
  )

(define (find-longest-major ll)
  (foldr (lambda (x y) (if (> (length x)
                              (length y)
                              )
                           x
                           y
                           )
           )
         '()
         (filter is-major? (all-sublists ll))
         )
  )