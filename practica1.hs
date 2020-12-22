--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 1
maximo :: Integer -> Integer -> Integer
maximo x y = if x <= y then y else x

maximoc :: (Integer, Integer) -> Integer
maximoc (x,y) = maximo x y

-- EJERCICIO 2
areaCirculo :: Float -> Float
areaCirculo r = (22 / 7) * r^2

-- EJERCICIO 3
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1 
fibonacci n = fibonacci(n-2) + fibonacci(n-1)

-- EJERCICIO 4
absol :: Integer -> Integer
absol x = if x < 0 then -x else x

-- EJERCICIO 5
cuadrado :: Integer -> Integer 
cuadrado x = x*x

sucesor :: Integer -> Integer
sucesor x = x+1

aumentar, aumentar2 :: Integer -> Integer
aumentar x = cuadrado (x+1)
aumentar2 x = (cuadrado x) + 1

aumentar', aumentar2' :: Integer -> Integer
aumentar' = cuadrado . sucesor
aumentar2' = sucesor . cuadrado

-- EJERCICIO 6
nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd x y = True

nAnd' :: Bool -> Bool -> Bool
nAnd' x y
        | x && y = False
        | otherwise = True

-- EJERCICIO 7
xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor x y = True

xor' :: Bool -> Bool -> Bool
xor' x y
        | x && y = False
        | not x && not y = False
        | otherwise = True

-- EJERCICIO 8
minimo :: Integer -> Integer -> Integer
minimo x y = if x <= y then x else y

minimoTres :: Integer -> Integer -> Integer -> Integer
minimoTres x y z = if (minimo x y) == x then minimo x z else minimo y z

-- EJERCICIO 9
maximoTres :: Integer -> Integer -> Integer -> Integer
maximoTres x y z
    | x <= y = maximo y z
    | otherwise = maximo x z

-- EJERCICIO 10
entre :: Integer -> Integer -> Integer -> Bool
entre x y z = (x<=y && y <= z) || (z<=y && y<=x)

numeroCentral :: Integer -> Integer -> Integer -> Integer
numeroCentral x y z
    | entre y x z = x
    | entre x y z = y
    | otherwise = z

-- EJERCICIO 11
productoRango :: Integer -> Integer -> Integer
productoRango x y
    | x > y = 0
    | x == y = x
    | otherwise = x * productoRango (x+1) y

fact :: Integer -> Integer
fact 0 = 1
fact x = productoRango 1 x

-- EJERCICIO 12
prod :: Int -> Int -> Int
prod x y
    | x == 0 || y == 0 = 0
    | otherwise = x + prod x (y-1)