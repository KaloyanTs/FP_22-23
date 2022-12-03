-- >>> x
-- 2

x = 2

fact n = if n == 0 then 1 else n * fact (n -1)

-- Pattern matching
f 0 = 1
f n = n * fact (n -1)

fastPow x 0 = 1
fastPow x n
  | even n = sq (fastPow x (div n 2))
  | otherwise = x * sq (fastPow x (div n 2))
  where
    sq x = x * x

fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

complAdd (a, b) (c, d) = (a + c, b + d)

complSub (a, b) (c, d) = (a - c, b - d)

complMul (a, b) (c, d) = (a * c - b * d, b * c + a * d)

compose f g x = f (g x)

sq x = x * x

inc x = x + 1