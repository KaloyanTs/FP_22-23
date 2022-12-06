import Prelude hiding (gcd,lcm,(.),length,maximum,map,filter,foldr,id,(<*>))

gcd 0 0 = error "undefined"
gcd x 0 = x
gcd 0 y = y
gcd x y 
   | x > y = gcd (x - y) y 
   | otherwise = gcd x (y - x)

cylinderVolume r h = pi * r * r * h
    where pi = 4 * atan 1

countDigits :: Int -> Int
countDigits n
   | n<10 = 1
   | otherwise = 1 + countDigits (n `div` 10)