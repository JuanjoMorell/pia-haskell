--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-} 

----------------------------------------------------
-----------------------------------------EJERCICIO 1 
----------------------------------------------------

maximo, maximo' :: (Integer, Integer) -> Integer
maximo(x,y) = if x >= y then x else y

maximo'(x,y)
    | x<y = y
    | otherwise = x

maximoc, maximoc' :: Integer -> Integer -> Integer
maximoc x y = if x >= y then x else y

maximoc' x y
    | x<y = y
    | otherwise = x

----------------------------------------------------
-----------------------------------------EJERCICIO 2 
----------------------------------------------------

areaCirculo :: Float -> Float
areaCirculo n = (22/7) * n^2

----------------------------------------------------
-----------------------------------------EJERCICIO 3 
----------------------------------------------------

fib2 :: Integer -> Integer
fib2 0 = 0
fib2 1 = 1
fib2 n = fib2 (n-1) + fib2 (n-2)

----------------------------------------------------
-----------------------------------------EJERCICIO 4 
----------------------------------------------------

absol :: Integer -> Integer
absol n = if n<0 then -n else n

----------------------------------------------------
-----------------------------------------EJERCICIO 5 
----------------------------------------------------

aumentar, aumentar2 :: Integer -> Integer
aumentar n = (n+1)^2
aumentar2 n = (n^2)+1

sucesor :: Integer -> Integer
sucesor = (+1)

cuadrado :: Integer -> Integer
cuadrado n = n * n

aumentar':: Integer -> Integer
aumentar' = cuadrado.sucesor

aumentar2' :: Integer -> Integer
aumentar2' = sucesor.cuadrado

----------------------------------------------------
-----------------------------------------EJERCICIO 6 
----------------------------------------------------

nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd x y = True

nAnd' :: Bool -> Bool -> Bool
nAnd' x y
    | x && y = False
    | otherwise = True

----------------------------------------------------
-----------------------------------------EJERCICIO 7 
----------------------------------------------------

xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor x y = False

xor' :: Bool -> Bool -> Bool
xor' False x = x
xor' True x = not x

----------------------------------------------------
-----------------------------------------EJERCICIO 8 
----------------------------------------------------

minimo :: Integer -> Integer -> Integer
minimo x y = if x < y then x else y

minimoTres :: Integer -> Integer -> Integer -> Integer
minimoTres x y z = if t < z then t else z
    where t = minimo x y

----------------------------------------------------
-----------------------------------------EJERCICIO 9
----------------------------------------------------

maxTres, maxTres' :: Integer -> Integer -> Integer -> Integer
maxTres x y z
    | x >= y && x <= z = x
    | y >= z = y
    | otherwise = z

maxTres' x y z = max (max x y) z

----------------------------------------------------
----------------------------------------EJERCICIO 10
----------------------------------------------------

entre :: Integer -> Integer -> Integer -> Bool
entre x y z = (x <= y && y <= z) || (z<=y && y <= x)

numeroCentral :: Integer -> Integer -> Integer -> Integer
numeroCentral x y z
    | entre y x z = x
    | entre x y z = y
    | otherwise = z

----------------------------------------------------
----------------------------------------EJERCICIO 11
----------------------------------------------------

productoRango :: Integer -> Integer -> Integer
productoRango n m
    | m > n = 0
    | m == n = m
    | otherwise = m * productoRango (m+1) n

fact :: Integer -> Integer
fact 0 = 1
fact n = productoRango 1 n

----------------------------------------------------
----------------------------------------EJERCICIO 12
----------------------------------------------------

prod :: Integer -> Integer -> Integer
prod x y
    | x == 0 || y == 0 = 0
    | otherwise = x + prod x (y-1)