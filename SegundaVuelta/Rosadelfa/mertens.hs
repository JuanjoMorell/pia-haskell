--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Rosadelfa
import Data.Char

-- EJERCICIO 1
type Valor = (Integer,Integer)
type Adelfa = RAdelfa Valor

-- EJERCICIO 2
godel :: Integer -> Integer
godel n = product (zipWith (^) primos (digitos n))

primos :: [Integer]
primos = [p | p <- [2..], primo p]

divisible :: Integer -> Integer -> Bool
divisible x y = x `rem` y == 0

divisores :: Integer -> [Integer]
divisores x = [y | y <- [1..x], divisible x y]

primo :: Integer -> Bool
primo x = divisores x == [1,x]

-- EJERCICIO 3
digitos :: Integer -> [Integer]
digitos n
	| n==0 =[]
	| otherwise = digitos (n `div` 10)++[n `rem` 10]

digitos' :: Integer -> [Integer]
digitos' n = [toInteger(ord(x) - ord('0')) | x <- num]
    where num = show n

-- EJERCICIO 4
pots :: Integer -> [Integer]
pots n = [n^x | x <- [0,1..]]

-- EJERCICIO 5
gArbol :: Int -> Adelfa
gArbol k = podar (construirArbol potPrimos (0,1))
    where potPrimos = map pots (take k primos)
          construirArbol [] x = Nodo x []
          construirArbol (ps:pss) x = Nodo x (map (construirArbol pss) (valores ps x))
          valores ps (p,g) = zip (map (m+) ds) (cortar (map (g*) ps))
            where m = 10*p
          cortar = takeWhile (< 10^k)
          ds = [0..9]

mertens :: Int -> [Integer]
mertens n = [n | (n,g) <- aplanar (gArbol n), n == g]