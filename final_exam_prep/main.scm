#lang racket

(define empty-tree '())
(define empty? null?)
(define make-tree cons)
(define (leaf? t)
  (and (not(empty? t))
       (empty? (cdr t))))
(define root car)
(define children cdr)

(define (count p l)
  (length (filter p l))
  )

(define (any p l)
  (if (null? l)
      #f
      (or (p (car l))
          (any p (cdr l))
          )
      )
  )

(define (minPredecessor t x)
  (define (exists tree)
    (cond
      ((empty? tree) #f)
      ((= (root tree) x) #t)
      ( else (any (lambda (t) (exists t)) (children tree)))
      )
    )
  (define cnt (count exists (children t)))
  (cond
    ((empty? t) #f)
    ((= (root t) x) (root t))
    (( > cnt 1) (root t))
    ((= cnt 1) (minPredecessor (car (filter exists (children t)))))                  (else #f)
    )
  )

(define (exists tree val)
  (cond
    ((empty? tree) #f)
    ((= (root tree) val) #t)
    ( else (any (lambda (t) (exists t val)) (children tree)))
    )
  )

(define test (make-tree 3
                        (list (make-tree 4
                                         (list (make-tree 5
                                                          empty-tree)
                                               (make-tree 2 empty-tree)
                                               )
                                         )
                              (make-tree 3
                                         (list (make-tree 4
                                                          (list (make-tree 6 empty-tree)))
                                               (make-tree 5 empty-tree)))))
  )