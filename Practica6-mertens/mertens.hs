--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Rosadelfa

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
	| otherwise = digitos (n `div` 10) ++ [n `rem` 10]

-- EJERCICIO 4
pots :: Integer -> [Integer]
pots n = [n^x | x <- [0,1..]]

-- EJERCICIO 5
gArbol :: Int -> Adelfa
gArbol k = podar (construirArbol potPrimos (0,1) )
		where potPrimos = map pots (take k primos)
		      construirArbol [] x = Nodo x []
		      construirArbol (ps:pps) x = Nodo x (map (construirArbol pps) (valores ps x))
		      valores ps (n,g) = zip (map (m+) ds) (cortar(map (g*) ps))
		     				where m = 10*n
		      cortar = takeWhile (< 10^k)
		      ds = [0..9]

mertens :: Int -> [Integer]
mertens x = [n | (n,g) <- aplanar (gArbol x), n==g]		  