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
porcentaje n m = (fromIntegral n / fromIntegral m) * 100

-- EJERCICIO 6
soloLetras :: String -> String
soloLetras [] = []
soloLetras (x:xs) = if isUpper x || isLower x then x:soloLetras xs else soloLetras xs

-- EJERCICIO 7
ocurrencias :: Char -> String -> Int
ocurrencias c xs = length [x | x <- xs, x == c]

-- EJERCICIO 8
frecuencias :: String -> [Float]
frecuencias xs = [porcentaje (ocurrencias c texto) n | c <- ['a'..'z']]
    where texto = [toLower x | x <- xs]
          n = length (soloLetras xs)

-- EJERCICIO 9
chiCuad :: [Float] -> [Float] -> Float
chiCuad os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

-- EJERCICIO 10
rota :: Int -> [a] -> [a]
rota n xs = drop n xs ++ take n xs

-- EJERCICIO 11
descifra :: Mensaje -> Mensaje
descifra ms = codifica (-factor) ms
    where factor = head (posiciones (minimum tabChi) tabChi)
          tabChi = [chiCuad (rota n tabla') tabla | n <- [0..25]]
          tabla' = frecuencias ms 

posiciones :: Eq a => a -> [a] -> [Int]
posiciones x xs = 
    [i | (x',i) <- zip xs [0..], x == x']

tabla :: [Float]
tabla = [12.53, 1.42, 4.68, 5.86, 13.68, 0.69, 1.01, 
          0.70, 6.25, 0.44, 0.01,  4.97, 3.15, 6.71, 
          8.68, 2.51, 0.88, 6.87,  7.98, 4.63, 3.93, 
          0.90, 0.02, 0.22, 0.90,  0.52]