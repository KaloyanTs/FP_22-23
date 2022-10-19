#lang racket

(define const
  (lambda (i)
    (lambda (x) i)
    )
  )

(define forever-21 (const 21))

(define flip
  (lambda (f)
    (lambda (x y) (f y x))
    )
  )

(define f (flip -))

(define complement
  (lambda (f)
    (lambda (x) (not (f x))
      )
    )
  )

(define (less-than-5? x) (< x 5))

(define g (complement less-than-5?))

(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))
    )
  )

(define h (compose (lambda (x) (+ x 1)) (lambda (x) (* x x)))) ; ((x^2)+1)

(define repeat
  (lambda (n f)
    (if (zero? n)
        (lambda (x) x)
        (compose f (repeat (- n 1) f))
        )
    )
  )

(define plus4 (repeat 4 (lambda (x) (+ x 1))))

(define dx 0.00000001)

(define derive
  (lambda (f)
    (lambda (x) (/ (- (f (+ x dx)) (f x)) dx))
    )
  )

;todo
;(define derive-n
;  (lambda (n)
;    (repeat n derive)
;    )
;  )

(define (derive-n n f)
  (if (= n 1)
      (derive f)
      (derive (derive-n (- n 1) f)
              )
      )
  )

(define two (derive-n 2 (lambda (x) (* x x))))

(define 2* (derive (lambda (x) (* x x))))

(define (twist k f g)
  (repeat (quotient k 2) (compose f g))
  )

(define (++ x) (+ x 1))
(define (sq x) (* x x))
(define foo (twist 4 ++ sq))
; това ще смята ((((x^2)+1)^2)+1)
(define bar (twist 2 ++ sq))
; това ще смята ((x^2)+1)

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))
      )
  )

(define (accumulateIter op nv a b term next)
  (define (iter i r)
    (if (> i b) r
        (iter (next i) (op r (term i)))
        )
    )
  (iter a nv)
  )

(define !!
  (lambda (n)
    (accumulate *
                1
                (if (even? n) 2 1)
                n
                (lambda (x) x)
                (lambda (x) (+ x 2))
                )
    )
  )

(define fact
  (lambda (n)
    (accumulate *
                1
                1
                n
                (lambda (x) x)
                (lambda (x) (+ x 1))
                )
    )
  )

(define (nchk-fact n k)
  (/ (fact n) (fact k) (fact (- n k)))
  )

(define (nchk n k)
  (accumulate *
              1
              1
              k
              (lambda (i) (/ (- (+ n 1) i) (- (+ k 1) i)))
              (lambda (i) (+ i 1))
              )
  )

(define x^
  (lambda (x)
    (lambda (n)
      (accumulate *
                  1
                  1
                  n
                  (lambda (t) x)
                  (lambda (t) (+ t 1))
                  )
      )
    )
  )

(define 2^ (x^ 2))

(define (all? p? a b)
  (accumulate (lambda (x y) (and x y))
              #t
              a
              b
              p?
              (lambda (x) (+ x 1))
              )
  )

(define (any? p? a b)
  (accumulate (lambda (x y) (or x y))
              #f
              a
              b
              p?
              (lambda (x) (+ x 1))
              )
  )

(define (filter-accum p? op nv a b term next)
  (cond ((> a b) nv)
        ((p? a) (op (term a)
                    (filter-accum p? op nv (next a) b term next)))
        (else       (filter-accum p? op nv (next a) b term next))))

(define (filter-accum-iter p? op nv a b term next)
  (define (iter i r)
    (cond ((> i b) r)
          ((p? i) (iter (next i) (op r (term i))))
          (else (iter (next i) r))
          )
    )
  (iter a nv)
  )

(define (divisors-sum n)
  (+ n (filter-accum (lambda (i) (zero? (remainder n i)))
                     +
                     0
                     1
                     (quotient n 2)
                     (lambda (x) x)
                     (lambda (x) (+ x 1))
                     )
     )
  )

(define (count p? a b)
  (filter-accum p?
                +
                0
                a
                b
                (lambda (i) (if (p? i) 1 0))
                (lambda (x) (+ x 1))
                )
  )

(define (prime? n)
  (filter-accum (lambda (i)  (zero? (remainder n i)))
                (lambda (x y) #f)
                #t
                2
                (sqrt n)
                (lambda (i) i)
                (lambda (i) (+ i 1))
                )
  )

(define (prime-all? n)
  (all? (lambda (x) (not (zero? (remainder n x)))) 2 (sqrt n))
  )

(define (repeat-accum n f)
  (accumulate compose
              (lambda (i) i)
              1
              n
              (lambda (i) f)
              (lambda (i) (+ i 1))
              )
  )