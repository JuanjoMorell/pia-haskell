--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Char
import System.IO
import Practica6

recogerdatos :: String -> [Integer]
recogerdatos "" = []
recogerdatos s = (ordenarConArbol (map read (words s)))

percentil :: (Integral a) => a -> [a] -> Int
percentil d [] = 0
percentil d xs = (m*100) `div` n
    where m = length [x | x <- xs, (x) <= (d)]
          n = length [x | x <- xs, (x) > (d)]

main = do putStr("Introduce el nombre del fichero de entrada -> ")
          entrada <- getLine
          datosEntrada <- readFile entrada
          putStr("Introduce el numero para realizar el percentil -> ")
          d <- getLine
          putStrLn("El resultado del percentil es: ")
          print(percentil (read d) (recogerdatos datosEntrada))
          putStrLn("Lista ordenada de los elementos del fichero de entrada:")
          putStrLn (show (recogerdatos datosEntrada))

