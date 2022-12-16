#lang racket

(define empty '())
(define left-tree cadr)
(define right-tree caddr)
(define root car)
(define empty? null?)
(define (isLeaf? t)
  (and (not (empty? t))
       (empty? (left-tree t))
       (empty? (right-tree t))
       )
  )
(define make-tree list)
(define (make-leaf x) (make-tree x empty empty))

(define (grow t x)
  (cond ((empty? t) t)
        ((isLeaf? t) (make-tree (root t)
                                (make-leaf x)
                                (make-leaf x)
                                )
                     )
        (else (make-tree (root t)
                         (grow (left-tree t) x)
                         (grow (right-tree t) x)
                         )
              )
        )
  )

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t) (cons h (delay t)))))

(define head car)
(define (tail s) (force (cdr s)))
(define empty-stream? null?)

(define (helper last n) (cons-stream
                         last
                         (helper (grow last n)
                                 (+ n 1)
                                 )
                         )
  )

(define infTrees (helper (make-leaf 0) 1))