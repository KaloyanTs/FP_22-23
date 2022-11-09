#lang racket

(define (deep-repeat l)
  (deep-repeat-lvl 1 l)
  )

(define (atom? a)
  (not (or (null? a)
           (pair? a)
           )
       )
  )

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op
                      nv
                      (next a)
                      b
                      term
                      next
                      )
          )
      )
  )

(define (dupl n el)
  (accumulate cons
              '()
              1
              n
              (lambda (i) el)
              (lambda (i) (+ i 1))
              )
  )

(define (deep-repeat-lvl lvl l)
  (cond ((null? l) '())
        ((atom? (car l)) (append (dupl lvl
                                       (car l)
                                       )
                                 (deep-repeat-lvl
                                  lvl
                                  (cdr l)
                                  )
                                 )
                         )
        (else (cons (deep-repeat-lvl (+ lvl 1)
                                     (car l)
                                     )
                    (deep-repeat-lvl lvl
                                     (cdr l)
                                     )
                    )
              )
        )
  )


(define (reverse n)
  (define (loop n res)
    (if (= n 0)
        res
        (loop (quotient n 10)
              (+ (* 10 res) (remainder n 10))
              )
        )
    )
  (loop n 0)
  )

(define (palindrome? n)
  (= n (reverse n))
  )

(define (sum-digits-divisors n)
  (define (iter res i)
    (if (zero? i)
        res
        (iter (if (or (zero? (remainder i
                                        10
                                        )
                             )
                      (zero? (remainder n
                                        (remainder i
                                                   10
                                                   )
                                        )
                             )
                      )
                  (+ res (remainder i 10))
                  res
                  )
              (quotient i 10)
              )
        )
    )
  (iter 0 n)
  )

(define (decart-inc a b)
  (accumulate append
              '()
              a
              (- b 1)
              (lambda (i) (accumulate cons
                                      '()
                                      (+ i 1)
                                      b
                                      (lambda (j) (cons i j))
                                      (lambda (j) (+ j 1))
                                      )
                )
              (lambda (i) (+ i 1))
              )
  )

(define (same-sum a b)
  (length (filter (lambda (x) (= (sum-digits-divisors (car x))
                                 (sum-digits-divisors (cdr x))
                                 )
                    )
                  (decart-inc a b)
                  )
          )
  )
