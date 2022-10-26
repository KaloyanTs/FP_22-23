#lang racket

(define (take n lst)
  (cond ((zero? n) '())
        ((>= n (length lst)) lst)
        (else (cons (car lst) (take (- n 1) (cdr lst))))
        )
  )

(define (drop n lst)
  (cond ((zero? n) lst)
        ((>= n (length lst)) '())
        (else (drop (- n 1) (cdr lst)))
        )
  )

(define (all? p? lst)
        (or (null? lst)
            (and (p? (car lst)) (all? p? (cdr lst)))
            )
  )

(define (any? p? lst)
        (and (not (null? lst))
             (or (p? (car lst) (all? p? (cdr lst))))
        )
  )