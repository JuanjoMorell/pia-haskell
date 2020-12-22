--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 1
-- multiplicar(0, infinito) -> 0
-- multiplicar(infinito, 0) -> infinito

-- EJERCICIO 2
-- funny1 f = 1 + f0 | funny1 :: (Integer -> Integer) -> Integer
-- funny2 f x = x + f(fx) | funny2 :: (Integer -> Integer) -> Integer -> Integer

-- EJERCICIO 3
-- curry (uncurry f) x y = f x y
-- (uncurry f) x y
-- f x y

-- uncurry (curry f) (x,y) = f (x,y)
-- (curry f) x y
-- f (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
curry :: ((a,b) -> c) -> (a -> b -> c)

-- EJERCICIO 4
delta :: Float -> Float -> Float -> Float

-- EJERCICIO 5
-- (*)x = (*x) | Correcta 
-- (+)x = (x+) | Correcta
-- (-)x = (-x) | Incorrecta

-- EJERCICIO 6
-- Funcion . -> Composicion
f :: Integer -> Integer
g :: Integer -> (Integer -> Integer)
h :: Integer -> Integer -> (Integer -> (Integer -> Integer -> Integer))
h x y = f (g x y)
