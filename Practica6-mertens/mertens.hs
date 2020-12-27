--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Rosadelfa

type Valor = (Integer,Integer)
type Adelfa = RAdelfa Valor

godel :: Integer -> Integer
godel n = product (zipWith (^) primos (digitos n))

divisible :: Integer -> Integer -> Bool
divisible x y = x `rem` y == 0

divisores :: Integer -> [Integer]
divisores x = filter (divisible x) [1..x]

primos :: Integer -> [Integer]
primos n = [x | x <- [2..], length (divisores x) == 2]

digitos :: Integer -> [Integer]
digitos n
	| n==0 =[]
	| otherwise = digitos (n `div` 10)++[n `rem` 10]
