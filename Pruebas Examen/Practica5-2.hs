--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Char

-- EJERCICIO 1
type Mensaje = String

-- EJERCICIO 2
minusculaAint :: Char -> Int
minusculaAint c = ord (c) - ord('a')

mayusculaAint :: Char -> Int
mayusculaAint c = ord (c) - ord('A')

intAminuscula :: Int -> Char
intAminuscula n = toEnum(n + ord('a'))

intAmayuscula :: Int -> Char
intAmayuscula n = toEnum(n + ord('A'))

-- EJERCICIO 3
desplaza :: Int -> Char -> Char
desplaza n c
    | 'a' <= c && c <= 'z' = intAminuscula ((minusculaAint c + n) `mod` 26)
    | otherwise = intAmayuscula ((mayusculaAint c + n) `mod` 26)

-- EJERCICIO 4
codifica :: Int -> Mensaje -> Mensaje
codifica n msj = [desplaza n x | x <- msj]

-- EJERCICIO 5
porcentaje :: Int -> Int -> Float
porcentaje n m = ((fromIntegral n) / (fromIntegral m)) * 100

-- EJERCICIO 6
soloLetras :: String -> String
soloLetras = map toLower . concat . words

-- EJERCICIO 7
ocurrencias :: Char -> String -> Int
ocurrencias c xs = length [x | x <- xs', x == c]
    where xs' = soloLetras xs

-- EJERCICIO 8
frecuencias :: String -> [Float]
frecuencias xs = [porcentaje (ocurrencias (intAminuscula x) xs') total | x <- [0..25]]
    where total = length xs'
          xs' = soloLetras xs

-- EJERCICIO 9
chiCuad :: [Int] -> [Float] -> Float
chiCuad [] [] = 0
chiCuad (x:xs) (y:ys) = ((x' - y')^2 / y') + (chiCuad xs ys)
    where x' = fromIntegral x
          y' = fromIntegral y

-- EJERCICIO 10
rota :: Int -> String -> String
rota n xs = drop n xs ++ take n xs

-- EJERCICIO 11
tabla :: [Float]
tabla = [12.53, 1.42, 4.68, 5.86, 13.68, 0.69, 1.01, 
          0.70, 6.25, 0.44, 0.01,  4.97, 3.15, 6.71, 
          8.68, 2.51, 0.88, 6.87,  7.98, 4.63, 3.93, 
          0.90, 0.02, 0.22, 0.90,  0.52]

descifra :: Mensaje -> Mensaje
descifra msj = codifica (-factor) msj
    where factor = head(posiciones (minimum chiValores) chiValores)
          chiValores = [chiCuad (rota n tabla') tabla | n <- [0..25]]
          tabla' = frecuencias msj

posiciones :: Eq a => a -> [a] -> [Int]
posiciones x xs = 
    [i | (x',i) <- zip xs [0..], x == x']