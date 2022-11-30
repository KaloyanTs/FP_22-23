#lang racket

(define empty-stream? null?)
(define (force e) (e))
(define-syntax delay
  (syntax-rules () ((delay x) (lambda () x))))


(define the-empty-stream '())
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t)
     (cons h (delay t))
     )
    )
  )

(define head car)
(define (tail s) (force (cdr s)))

(define (gen-stream nv1 nv2 op)
  (cons-stream nv1 (gen-stream nv2 (op nv1 nv2) op))
  )

(define fibs (gen-stream 0 1 +))

(define (take n s)
  (if (or (= n 0) (empty-stream? s)) '()
      (cons (head s) (take (- n 1) (tail s)))))
