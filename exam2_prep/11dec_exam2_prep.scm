#lang racket

(define (accumulate op
                    nv
                    a
                    b
                    term
                    next
                    )
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

(define (isNPerm n f)
  (define (iter2 i j)
    (if (> j (- n 1))
        #t
        (and (not (= (f i) (f j)))
             (iter2 i (+ j 1))
             )
        )
    )
  (define (iter i)
    (iter2 i (+ i 1))
    )
  (and
   (accumulate (lambda (x y) (and x y))
               #t
               0
               (- n 1)
               (lambda (x) (and (>= (f x) 0) (< (f x) n)))
               (lambda (x) (+ x 1))
               ) 
   (accumulate (lambda (x y) (and x y))
               #t
               0
               (- n 1)
               (lambda (x) (iter x))
               (lambda (x) (+ x 1))
               )
   )
  )