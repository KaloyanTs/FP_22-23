# Упражнение 8 - Мързеливо оценяване в Scheme. Въведение в Хаскел

[код от упражнението](ex08-20211203-solutions.hs)

### Зад.0.
Какво е мързеливо оценяване? Дайте примери за специални форми. Напишете своя :)

### Зад.1.
Да се напише функцията `fibonacci`, която изчислява `n`-тото число на Фибоначи:
```haskell
fibonacci 5   -> 5
fibonacci 40  -> 102334155
fibonacci 400 -> 176023680645013966468226945392411250770384383304492191886725992896575345044216019675
```
### Зад.2.
Напишете горната функция по колко може повече начини.

### Зад.3.
Да се напише функция `fastPow`, която степенува реално число `x` на неотрицателна степен `n` по метода на бързото степенуване

### Зад.4.
Да се напишат функции complAdd, complSub и complMul, които извършват съответните операции над комплексни числа, представени като наредени двойки от координатите си:
```haskell
complAdd (1,2) (-3,5) -> (-2,7)
complSub (4,8) (2,-1) -> (2,9)
complMul (3,5) (2,1) -> (1,13)
```
### Зад.5.
Да се напише функция `distance`, която намира разстоянието между две точки в равнината (наредени двойки):
```haskell
distance (-2,3) (1,7) -> 5
distance (0,0) (1,1) -> 1.4142135623730951
```
### Зад.6.
Нека `f` е функция на един аргумент и `n` е цяло неотрицателно число.
Дефинираме `n`-тото прилагане на функцията `f` да бъде функцията, дефинирана
по следния начин:

   f<sup>0</sup>(x) = x

   f<sup>n</sup>(x) = f(f<sup>n-1</sup>(x))

Да се напише функция `repeated`, която връща `n`-тото прилагане на `f`.
```haskell
let foo = repeated succ 10 in foo 5 -- 15
```