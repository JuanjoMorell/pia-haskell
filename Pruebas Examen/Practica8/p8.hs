--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import ListaPar
import Data.Char

-- EJERCICIO 1
type ListaPCh = ListaP2 Char

-- EJERCICIO 2
listaAcadena :: ListaPCh -> String
listaAcadena [] = ""
listaAcadena ((i,a):xs) = show i ++ [a] ++ listaAcadena xs

-- EJERCICIO 3
cadenaComprimida :: String -> String
cadenaComprimida = listaAcadena . comprimida

-- EJERCICIO 4
cadenaAlista :: String -> ListaPCh
cadenaAlista [] = []
cadenaAlista xs = [(read (takeWhile isDigit xs), head (dropWhile isDigit xs))] ++ cadenaAlista (tail (dropWhile isDigit xs))

-- EJERCICIO 5
cadenaExpandida :: String -> String
cadenaExpandida = expandida . cadenaAlista

-- EJERCICIO 6
main :: IO()
main = do putStrLn "PROGRAMA PARA CODIFICAR/DECODIFICAR"
          putStrLn "(1) Codificar"
          putStrLn "(2) Decodificar"
          putStrLn "(3) Salir"
          opcion <- getLine
          case opcion of
              "1" -> do putStr "Introduzca cadena a codificar: "
                        cadena <- getLine
                        putStrLn (cadenaComprimida cadena)
                        main
              "2" -> do putStr "Introduzca cadena a decodificar: "
                        cadena <- getLine
                        putStrLn (cadenaExpandida cadena)
                        main
              "3" -> return()