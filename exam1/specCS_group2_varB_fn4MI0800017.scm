#lang r5rs

(define (accumulate op nv a b  term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next)
          )
      )
  )

(define (foldr op nv l)
  (if (null? l) nv (op (car l) (foldr op nv (cdr l))))
  )

(define id (lambda (x) x))
(define ++ (lambda (x) (+ x 1)))

(define (all? p? a b)
  (accumulate (lambda (x y) (and (p? x) y))
              #t
              a
              b
              id
              ++
              )
  )

(define (isPrime? n)
  (and (> n 1)
       (all? (lambda (i) (not (zero? (remainder n i))))
             2
             (sqrt n)
             )
       )
  )

(define (grow n)
  (define up (quotient n 2))
  (define (iter res i)
    (if (> i up) res
        (iter (if (and (isPrime? i) (zero? (remainder n i)))
                  (* res i)
                  res
                  )
              (++ i)
              )
        )
    )
  (iter n 2)
  )

(define (gcd a b)
  (cond ((= a b) a)
        ((> a b) (gcd (- a b) b))
        (else (gcd a (- b a)))
        )
  )

(define (isUnitary? d n)
  (= (gcd d (quotient n d)) 1)
  )

(define (maxUnitary n)
  (accumulate (lambda (x y) (if (and (zero? (remainder n x))
                                     (isUnitary? x n)
                                     )
                                (max x y)
                                y
                                )
                )
              1
              2
              (quotient n 2)
              id
              ++
              )
  )

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l)))
  )

(define (reverse l)
  (foldl (lambda (x  y) (cons y x)) '() l)
  )

(define (determineRule2 f x y prev)
  (cond ((and (< (f x) (f y)) (< (f y) (min x y))) 1)
        ((and (> (f y) (f x)) (> (f x) (max x y))) 2)
        (else prev) 
        )
  )

(define (selectiveMap f a b)
  (define (determineRule f x y prev)
    (cond ((and (< (f x) (f y)) (< (f y) (min x y))) 1)
          ((and (> (f y) (f x)) (> (f x) (max x y))) 2)
          (else prev) 
          )
    )
  (define (iter res prev l1 l2)
    (define rule (if (null? l1)
                     #f
                     (determineRule f (car l1)
                                    (car l2)
                                    prev
                                    )
                     )
      )
    (if (null? l1)
        res
        (cond ((= rule 1)
               (iter (cons (f (car l1)) res)
                     rule
                     (cdr l1)
                     (cdr l2)
                     )
               )
              ((= rule 2)
               (iter (cons (f (car l2)) res)
                     rule (cdr l1) (cdr l2)))
              (else #f)
              )
        )
    )
  (reverse (if (null? a)
               '()
               (iter (list (f (car a))) 1 (cdr a) (cdr b))
               )
           )
  )

(define (filter p? l)
  (foldr (lambda (x r) (if (p? x) (cons x r) r)) '() l)
  )

(define (any? p? l)
  (foldr (lambda (x y) (or (p? x) y))
         #f
         l
         )
  )

(define (commonFreq device network)
  (filter (lambda (x) (any? (lambda (y) (= x y))
                            network
                            )
            )
          device
          )
  )

(define (isCompatible? device network)
  (> (length (commonFreq device network))
     1
     )
  )

(define (coverage device network)
  (define cmn (commonFreq device network))
  (/ (length cmn) (length network))
  )

(define (preferredDevice network deviceList)
  (foldr (lambda (x y) (if (> (/ (length x) (length network))
                              (/ (length y) (length network))
                              )
                           x
                           y
                           )
           )
         '()
         (filter (lambda (y) (>= (length y) 2))
                 (map (lambda (x) (commonFreq x network))
                      deviceList
                      )
                 )
         )
  )

(define (coverageSum device networkList)
  (foldr +
         0
         (map (lambda (x) (coverage device x))
              networkList
              )
         )
  )

(define (count el l)
  (foldr +
         0
         (map (lambda (x) (if (= el x) 1 0)) l)
         )
  )

(define (uniques l)
  (define (iter res lst)
    (if (null? lst) res
        (iter (if (= (count (car lst)
                            (cdr lst)
                            )
                     0
                     )
                  (cons (car lst) res)
                  res
                  )
              (cdr lst)
              )
        )
    )
  (reverse (iter '() l))
  )

(define (preferredDeviceForAll deviceList networkList)
  (let* ((modList (map (lambda (x)
                         (cons x
                               (foldr +
                                      0
                                      (map (lambda (y) (if (isCompatible? x y)
                                                           1
                                                           0
                                                           )
                                             )
                                           networkList
                                           )
                                      )
                               )
                         )
                       deviceList       
                       )
                  )
         (bstDevCount
          (foldl
           (lambda (x y)
             (if (> (cdr x) (cdr y))
                 x
                 (if (< (cdr x) (cdr y))
                     y
                     (if (> (coverageSum (car x) networkList)
                            (coverageSum (car y) networkList)
                            )
                         x
                         y
                         )
                     )
                 )
             )
           (car modList)
           (cdr modList)
           )
          )
         (bestDevice (car bstDevCount))
         (cmnNetworks (map (lambda (x) (commonFreq bestDevice x))
                           networkList)
                      ))
    (if (< (cdr bstDevCount) 2)
        '()
        (uniques (foldr append '() cmnNetworks))
        )
    )
  )