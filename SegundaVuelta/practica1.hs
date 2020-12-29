--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 1
maximo :: Integer -> Integer -> Integer
maximo x y
        | x <= y = y
        | otherwise = x

maximo' :: (Integer, Integer) -> Integer
maximo' (x,y) = if x <= y then y else x

-- EJERCICIO 2
areaCirculo :: Float -> Float
areaCirculo r = r*r*pi

-- EJERCICIO 3
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- EJERCICIO 4
absol :: Integer -> Integer
absol n = if n < 0 then (-n) else n

-- EJERCICIO 5
aumentar, aumentar2 :: Integer -> Integer
aumentar n = (n+1)^2
aumentar2 n = n^2 + 1

-- EJERCICIO 6
nAnd :: Bool -> Bool -> Bool
nAnd x y = if x && y then True else False

-- EJERCICIO 7
xor :: Bool -> Bool -> Bool
xor x y = if x == y then True else False

-- EJERCICIO 
minimo :: Integer -> Integer -> Integer
minimo x y = if x <= y then x else y

minimoTres :: Integer -> Integer -> Integer -> Integer
minimoTres x y z = minimo x (minimo y z)

-- EJERCICIO 9
maximoTres :: Integer -> Integer -> Integer -> Integer
maximoTres x y z
    | x > y && x > z = x
    | y > x && y > z = y
    | otherwise = z

-- EJERCICIO 10
numeroCentral :: Integer -> Integer -> Integer -> Integer
numeroCentral x y z
    | x > y && x < z = x
    | y > x && y < z = y
    | otherwise = z

-- EJERCICIO 11
productoRango :: Integer -> Integer -> Integer
productoRango n m
            | n < m = 0
            | n == m = m
            | otherwise = m * productoRango (m+1) n

fact :: Integer -> Integer
fact 0 = 1
fact n = productoRango 1 n

-- EJERCICIO 12
prod :: Int -> Int -> Int
prod x y 
	| x==0 || y==0 = 0
	| otherwise =  x + prod x (y-1)
