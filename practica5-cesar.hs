--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Char
import Data.List
import System.IO

-- EJERCICIO 1
type Mensaje = String

-- EJERCICIO 2
minusculasAint :: Char -> Int
minusculasAint c = ord c - ord 'a'

mayusculasAint :: Char -> Int
mayusculasAint c = ord c - ord 'A'

intAminusculas :: Int -> Char
intAminusculas i = chr (ord 'a' + i)

intAmayusculas :: Int -> Char
intAmayusculas i = chr (ord 'A' + i)

-- EJERCICIO 3
desplaza :: Int -> Char -> Char
desplaza 0 c = c
desplaza n c = if elem c ['a'..'z'] then intAminusculas(((minusculasAint c)+n) `mod` 27) else intAmayusculas(((mayusculasAint c)+n) `mod` 27)


-- EJERCICIO 4
codifica :: Int -> Mensaje -> Mensaje
codifica n msj = [desplaza n x | x <- msj]

-- EJERCICIO 5
porcentaje :: Int -> Int -> Float
porcentaje n m = (n `div` m) * 100

-- EJERCICIO 6


-- EJERCICIO 7


-- EJERCICIO 8


-- EJERCICIO 9


-- EJERCICIO 10


-- EJERCICIO 11