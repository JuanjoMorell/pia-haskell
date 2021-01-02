--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Char

-- EJERCICIO 1
type Mensaje = String

-- EJERCICIO 2
minusculaAint, mayusculaAint :: Char -> Int
minusculaAint c = ord(c) - ord('a')
mayusculaAint c = ord(c) - ord('A')

letraAint :: Char -> Int
letraAint c
    | 'a' <= c && c <= 'z' = minusculaAint c
    | 'A' <= c && c <= 'Z' = mayusculaAint c
    | otherwise = error "Caracter no permitido"

intAminuscula, intAmayuscula :: Int -> Char
intAminuscula n = toEnum(n+ ord('a')) 
intAmayuscula n = toEnum(n+ ord('A')) 

-- EJERCICIO 3
desplaza :: Int -> Char -> Char
desplaza n c 
    | elem c ['a'..'z'] = intAminuscula ((minusculaAint c+n) `mod` 26)
    | elem c ['A'..'Z'] = intAmayuscula ((mayusculaAint c+n) `mod` 26)
    | otherwise         = c

-- EJERCICIO 4
codifica :: Int -> Mensaje -> Mensaje
codifica n xs = [desplaza n x | x <- xs]

-- EJERCICIO 5
porcentaje :: Int -> Int -> Float
porcentaje n m = (fromIntegral n / fromIntegral m) * 100

-- EJERCICIO 6
soloLetras :: String -> String
soloLetras xs = [x | x <- xs, isLetter x]

-- EJERCICIO 7
ocurrencias :: Char -> String -> Int
ocurrencias c xs = length [x | x <- xs, x == c]

-- EJERCICIO 8
frecuencias :: String -> [Float]
frecuencias xs = [porcentaje (ocurrencias x xs') l | x <- ['a'..'z']]
    where l = length (soloLetras xs)
          xs' = map toLower xs

-- EJERCICIO 9
chiCuad :: [Float] -> [Float] -> Float
chiCuad xs ys = sum [((o-e)^2) / e | (o,e) <- zip xs ys]

-- EJERCICIO 10
rota :: Int -> [a] -> [a]
rota n xs = drop n xs ++ take n xs

-- EJERCICIO 11
tabla :: [Float]
tabla = [12.53, 1.42, 4.68, 5.86, 13.68, 0.69, 1.01, 
          0.70, 6.25, 0.44, 0.01,  4.97, 3.15, 6.71, 
          8.68, 2.51, 0.88, 6.87,  7.98, 4.63, 3.93, 
          0.90, 0.02, 0.22, 0.90,  0.52]

descifra :: Mensaje -> Mensaje
descifra xs =  codifica (-factor) xs
 where
  factor = head (posiciones (minimum tabChi) tabChi)
  tabChi = [chiCuad (rota n tabla') tabla | n <- [0..25]]
  tabla' = frecuencias xs
 
posiciones :: Eq a => a -> [a] -> [Int]
posiciones x xs = 
    [i | (x',i) <- zip xs [0..], x == x']