Задача 1. Казваме, че една функция `π `е n-пермутация, ако тя е биекция на интервала от естествени числа `[0; n-1]` в себе си. Цикъл в пермутацията π наричаме редица от числа x1, … xk, така че `π(xi) = xi+1` за `i < k` и `π(xk) = x1`.

а) (4 т.) Да се реализира функция isNPerm, която приема като параметри естествено число n и едноместна числова функция f и проверява дали f е n-пермутация.

б) (6 т.) Да се реализира функция maxCycle, която по дадено число n и n-пермутация π намира максимален по дължина цикъл в π.

Примери:
```
isNPerm 3 (\x -> (3 - x) `mod` 3) → True
isNPerm 10 (`div` 2) → False
isNPerm 10 (\x -> (x + 2) `mod` 10) → True
maxCycle 3 (\x -> (3 - x) `mod` 3) → [1, 2]

maxCycle 10 (\x -> (x + 2) `mod` 10) → [0, 2, 4, 6, 8]
maxCycle 10 (\x -> (x + 3) `mod` 10) → [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
```
Задача 2. а) (5 т.) “Пълзяща средна стойност” (“moving average”) на редица от дробни числа S с прозорец n наричаме редицата от средни аритметични на n последователни елемента от S, където n е предварително фиксирано цяло число. Да се реализира функция movingAverage, която по даден безкраен поток от дробни числа S и естествено число n ≥ 2 връща безкрайния поток от пълзящата средна стойност на S с прозорец n.

б) (5 т.) Да се реализира функция allAverages, която по даден безкраен поток от дробни числа S връща безкраен поток от безкрайни потоци A2, A3, A4,... където An представлява пълзящата средна стойност на S с прозорец n.

Примери:
```
movingAverage [1076,1356,1918,6252,6766,5525, … ] 3 ↝ [1450.0,3175.3,4978.6,6181.0, …]

allAverages [1076, 1356, 1918, 6252, 6766, 5525, … ] ↝ [[1216.0,1637.0,4085.0,6509.0, …], [1450.0, 3175.3,4978.6,6181.0, …], [2650.5,4073.0,5115.25, … ], …]
```
Задача 3. Иван е много подреден човек и обича да систематизира принадлежностите си в кутии, като всичко е прилежно надписано. Той следи инвентара на личния си лаптоп, като го представя чрез списък от наредени двойки: етикет на кутия (произволен низ) и списък от съдържанието ѝ – етикети на предмети, сред които евентуално и други кутии (също произволни низове). Всички етикети са уникални. Иван има нужда от помощ с реализацията на някои функции за управление на инвентара си:

а) (4 т.) Да се реализира функция allObjects, която по даден инвентар връща списък от етикетите на всички предмети, които не са кутии.

б) (6 т.) Иван не обича прахосничеството и иска да разчисти ненужните кутии. Да се реализира функция cleanUp, която по даден инвентар връща негово копие, в което празните кутии са изхвърлени.

Упътване: Да се вземе предвид, че след изхвърляне на празни кутии, други кутии може да останат празни и те също трябва да се изхвърлят. Във върнатия резултат не трябва да има празни кутии.

Примери:
```

inv = [ ("docs", ["ids", "invoices"]), ("ids", ["passport"]),  ("invoices", []), ("memes", []),
        ("family", ["new year", "birthday"]), ("funny", ["memes"]), ("pics", ["family", "funny"]) ]

allObjects inv → ["passport", "new year", "birthday"]

cleanUp inv → [ ("docs", ["ids"]), ("ids", ["passport"]), ("family", ["new year", "birthday"]),
                ("pics", ["family"])]
```

Бонус: Да се разшири функцията cleanUp, така че кутиите, които съдържат само една кутия, да приемат нейното съдържание, но да запазят етикета си. След почистването да не останат кутии, които съдържат само по една кутия.

Пример за бонуса:

```
cleanUp inv → [ ("docs", ["passport"]), ("pics", ["new year", "birthday"])
```

Забележка: използването на всички стандартни функции в R5RS, както и на функциите accumulate, accumulate-i, filter, foldr, foldl, foldr1, foldl1, head, tail, map-stream, filter-stream и специалната форма cons-stream е позволено, но не е задължително.
```
(define (accumulate op nv a b term next)
  (if (> a b) nv
          (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
          (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (filter p l)
  (cond ((null? l) l)
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))

(define head car)

(define (tail s) (force (cdr s)))

(define (map-stream f . streams)
  (cons-stream (apply            f (map head streams))
               (apply map-stream f (map tail streams))))

(define (filter-stream p? s)
  (if (p? (head s))
      (cons-stream (head s) (filter-stream p? (tail s)))
                            (filter-stream p? (tail s))))
```