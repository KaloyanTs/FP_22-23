# Упражнение 2 – Функции от по-висок ред

[код от упражнението](ex02-20221019-solutions.rkt)

### Задача 0
Да се напишат следните функции от по-висок ред:
-  `(const c)`, която по дадена константа `c` връща функцията `f(x)=c`:
```
(define forever-21 (const 21))
(forever-21 5) -> 21
(forever-21 10) -> 21
((const 21) 10) -> 21
```
-  `(flip f)`, която приема двуместна функция като аргумент и връща същата функция, но с разменени места на нейните аргументи:
```
(define f (flip -))
(f 4 10) -> 6 ; = (- 10 4)
((flip f) 4 10) -> -6
```
- `(complement p)`, която по даден предикат връща неговото отрицание:
```
(define (less-than-5? x) (< x 5))
(define f (complement less-than-5?))
(f 3) ; => #f
(f 5) ; => #t
(f 7) ; => #t
```
- `(compose f g)`, която връща композицията на две дадени едноместни функции:
```
(define f (compose (lambda (x) (+ x 1)) (lambda (x) (* x x)))) ; ((x^2)+1)
(f 3) -> 10
```
### Задача 1
Да се напише функцията от по-висок ред `(repeat n f)`, която връща `n`-кратната композиция на дадена функция `f`:
```
(define f (repeat 5 (lambda (x) (+ x 1))))
(f 10) -> 15
((repeat 0 (lambda (x) (+ x 1))) 10) -> ?
```
### Задача 2
Да се напише функция `(derive f)`, която връща производната на дадена функция `f` (може да използвате `dx=0.000001` за прецизност)
### Задача 3
Да се напише функция `(derive-n n f)`, която връща `n`-тата производна на дадена функция `f`.
### Задача 4*
Да се напише функция `(twist k f g)`, която за дадени едноместни функции `f` и `g` и четно число `k` връща функция, еквивалентна на `f(g(f(g(...(x)...))))`, където общият брой извиквания на `f` и `g` е `k`.
```
(define (++ x) (+ x 1))
(define (sq x) (* x x))
(define foo (twist 4 ++ sq))
; това ще смята ((((x^2)+1)^2)+1)
(define bar (twist 2 ++ sq))
; това ще смята ((x^2)+1)
(foo 2) -> 26
(bar 2) -> 5
```
### Задача 5 [загрявка].
Напишете функцията `accumulate` и нейния итеративен вариант.
```
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))
```
### За следващите задачи е задължително използването на accumulate, по възможност точно по веднъж
### Задача 6
Да се напише функция `(!! n)`, която по дадено естествено число `n` изчислява `n!!` - произведението на всички числа, по-малки или равни на `n`, със същата четност:
```
(!! 5) -> 15    ; =1*3*5
(!! 10) -> 3840 ; =2*4*6*8*10
```
### Задача 7
Да се напише функция `(nchk n k)`, която за дадени естествени числа `n` и `k` изчислява биномния коефициент 'n над k', използвайки:
- `accumulate` индиректно от функцията факториел
- само `accumulate` 

; името идва от `n-choose-k` - по колко начина можем да изберем k неща измежду n

### Задача 8
Да се напише функция `(2^ n)`, която изчислява 2<sup>n</sup> (където `n` е естествено), използвайки:
- само `accumulate`
- `accumulate` + `nchk`

### Задача 9
Да се напишат функциите `(all? p? a b)` и `(any? p? a b)`, които проверяват дали даден предикат `p?` е изпълнен за всяко (съотв. за някое) число в интервала `[a;b]`
- Упътване: не можем да използваме вградените `and` и `or` във функции от по-висок ред, но можем да си дефинираме свои, напр. `(lambda (x y) (and x y)`.

### Задача 9½ [още загрявка]
Напишете филтриращия вариант на функцията `accumulate`, отново и като итеративен процес:
```
(define (filter-accum p? op nv a b term next)
  (cond ((> a b) nv)
        ((p? a) (op (term a)
                    (filter-accum p? op nv (next a) b term next)))
        (else       (filter-accum p? op nv (next a) b term next))))
```
### Задача 10
Да се напише функция `(divisors-sum n)`, която намира сумата на всички делители на естественото число `n`.
```
(divisors-sum 12) -> 28 ;1+2+3+4+6+12=28
```
### Задача 11
Да се напише функция `(count p? a b)`, която проверява за колко измежду числата в целочисления интервал `[a;b]` е верен предиката `p?`.

### Задача 12
Да се напише функция `(prime? n)`, която проверява дали дадено число е просто:
```
(prime? 1) -> #f ;(!)
(prime? 2) -> #t
(prime? 101) -> #t
```
### Задача 13*
Да се напише функцията от по-висок ред `(repeat n f)`, използвайки `accumulate`