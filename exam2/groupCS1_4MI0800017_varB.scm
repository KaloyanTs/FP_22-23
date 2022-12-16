#lang r5rs

(define root car)
(define left-tree cadr)
(define right-tree caddr)
(define empty? null?)
(define empty '())
(define make-tree list)
(define (make-leaf x) (make-tree x empty empty) )

;the binary tree is represented by a
;list with 3 elements
;these elements are its root,
;its left subtree and its right subtree

(define (sameTrace tree l)
  (define (trace x t path)
    (cond ((empty? t) path)
          (((root t) x) (trace x
                               (right-tree t)
                               (cons (root t) path)
                               )
                        )
          (else (trace x
                       (left-tree t)
                       (cons (root t) path)
                       )
                )
          )
    )
  (define (anySamePath p others)
    (cond ((null? others) #f)
          ((equal? (cadr p) (cadr (car others)))
           #t)
          (else 
           (anySamePath p (cdr others))
           )
          )
    )
  (define (helper lst)
    (if (null? lst) #f
        (or (anySamePath (car lst) (cdr lst))
            (helper (cdr lst))
            )
        )
    )
  (helper
   (map (lambda (x) (list x (trace x tree '()))) l))
  )

(define (all? p l)
  (if (null? l)
      #t
      (and (p (car l))
           (all? p (cdr l))
           )
      )
  )

(define (from-to a b)
  (if (> a b)
      '()
      (cons a (from-to (+ a 1) b))
      )
  )

;predicates
(define (prime? n)
  (and (> n 1)
       (all? (lambda (x)
               (not (zero? (remainder n x)))
               )
             (from-to 2 (quotient n 2))
             )
       )
  )
(define below2 (lambda (x) (< x 2)))
(define over0 (lambda (x) (> x 0)))
(define over6 (lambda (x) (> x 6)))
;odd?

;test tree
(define t (make-tree below2
                     (make-tree odd?
                                (make-leaf over6)
                                (make-leaf prime?)
                                )
                     (make-leaf over0)
                     )
  )