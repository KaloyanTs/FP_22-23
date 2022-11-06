#lang racket

(define (sum lst)
  (foldr + 0 lst)
  )

(define (last lst)
  (foldr (lambda (x y) (if (null? y) x y))
         '()
         lst
         )
  )

(define (append l1 l2)
  (foldr cons l2 l1)
  )

(define (push-back el lst)
  (foldr cons (list el) lst)
  )

(define (member? el lst)
  (foldr (lambda (x y) (or (eq? el x) y))
         #f
         lst
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

(define (from-to a b)
  (accumulate cons
              '()
              a
              b
              (lambda (x) x)
              (lambda (x) (+ x 1))
              )
  )

(define (reverse lst)
  (foldr push-back
         '()
         lst
         )
  )

(define (map f lst)
  (foldr (lambda (x y) (cons (f x) y))
         '()
         lst
         )
  )

(define (filter p? lst)
  (foldr (lambda (x y) (if (p? x)
                           (cons x y)
                           y
                           )
           )
         '()
         lst
         )
  )

(define (partition p? lst)
  (foldr (lambda (x y) (if (p? x)
                           (cons (cons x (car y))
                                 (list (car (cdr y)))
                                 )
                           (cons (car y)
                                 (list (cons x (car (cdr y))))
                                 )
                           )
           )
         (list '() '())
         lst
         )
  )

(define (isPrime? n)
  (and (> n 1)
       (accumulate (lambda (x y) (and x y))
                   #t
                   2
                   (sqrt n)
                   (lambda (i) (not (zero? (remainder n i))))
                   (lambda (i) (+ i 1))
                   )
       )
  )

(define (scp lst)
  (sum (map (lambda (x) (* x x x))
            (filter isPrime?
                    lst
                    )
            )
       )
  )

(define (foldl op nv lst)
  (define (iter l res)
    (if (null? l)
        res
        (iter (cdr l)
              (op res (car l))
              )
        )
    )
  (iter lst nv)
  )

(define (take n lst)
  (foldl (lambda (x y) (if (< (length x) n)
                           (push-back y x)
                           x
                           )
           )
         '()
         lst
         )
  )

(define (drop n lst)
  (if (> n (length lst))
      '()
      (accumulate (lambda (x y) (cdr y))
                  lst
                  1
                  n
                  (lambda (i) i)
                  (lambda (i) (+ i 1))
                  )
      )
  )

(define (all? p? lst)
  (foldr (lambda (x y) (and y (p? x)))
         #t
         lst
         )
  )

(define (any? p? lst)
  (not (all? (lambda (x) (not (p? x))) lst))
  )

(define (remove el lst)
  (reverse (cdr (foldl (lambda (x y) (if (and (not (car x)) (eq? el y))
                                         (cons #t (cdr x))
                                         (cons (car x) (cons y (cdr x)))
                                         )
                         )
                       (list #f)
                       lst
                       )
                )
           )
  )

(define (explode-digits n)
  (if (zero? n)
      (list 0)
      (reverse (accumulate cons
                           '()
                           (- 0 n)
                           -1
                           (lambda (i) (remainder (- 0 i) 10))
                           (lambda (i) (quotient i 10))
                           )
               )
      )
  )

(define (digit-occurence d n)
  (foldr +
         0
         (map (lambda (i) (if (eq? i d) 1 0)) (explode-digits n))
         )
  )

(define (remove-repeats lst)
  (reverse (foldl (lambda (x y) (if (and (not (null? x))
                                         (eq? y (car x))
                                         )
                                    x
                                    (cons y x)
                                    )
                    )
                  '()
                  lst   
                  )
           )
  )

(define (is-em? lst bin f)
  (and (all? (lambda (x) (member? (f x) lst)) lst)
       (all? (lambda (x) (all? (lambda (y) (eq? (f (bin x y))
                                                (bin (f x) (f y))
                                                )
                                 )
                               lst
                               )
               )
             lst
             )
       )
  )

(define (max-metric ml nl)
  (foldr (lambda (x y) (if (> (foldr + 0 (map x nl))
                              (foldr + 0 (map y nl))
                              )
                           x
                           y
                           )
           )
         (car ml)
         (cdr ml)
         )
  )