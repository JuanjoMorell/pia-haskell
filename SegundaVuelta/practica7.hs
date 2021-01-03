--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Char
import Practica6

convertir :: String -> [Int]
convertir xs = [ord x - ord '0'| x <- cadena]
    where cadena = concat (words xs)

calPercentil :: Int -> [Int] -> Int
calPercentil n xs = (m*100) `div` (length xs)
    where m = length [x | x <- xs, x <= n]

main = do putStr "Introduzca nombre del fichero de entrada: "
          fEntrada <- getLine
          contenido <- readFile fEntrada
          let numeros = ordenarConArbol (convertir contenido)
          putStrLn (show numeros)
          putStr "Introduzca dato para calcular el percentil: "
          d <- getLine
          let percentil = calPercentil (read d) numeros
          putStr "El percentil es: "
          putStrLn (show percentil)