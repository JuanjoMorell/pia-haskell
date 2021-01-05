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
listaAcadena ((i,e):xs) = show i ++ [e] ++ (listaAcadena xs)

-- EJERCICIO 3
cadenaComprimida :: String -> String
cadenaComprimida = listaAcadena . comprimida

-- EJERCICIO 4
cadenaAlista :: String -> ListaPCh
cadenaAlista [] = []
cadenaAlista xs = (read (takeWhile isDigit xs), head(dropWhile isDigit xs)) : (cadenaAlista (tail(dropWhile (isDigit) xs)))  

-- EJERCICIO 5
cadenaExpandida :: String -> String
cadenaExpandida = expandida . cadenaAlista

-- EJERCICIO 6
main :: IO()
main = do putStrLn "Bienvenido al programa para codificar o decodificar"
          putStrLn "Seleccione operacion:"
          putStrLn "(1) Codificar"
          putStrLn "(2) Decodificar"
          putStrLn "(3) Salir"
          op <- getLine
          case op of
              "1" -> do putStrLn "CODIFICAR. Introduzca cadena a codificar."
                        cad <- getLine
                        putStrLn (cadenaComprimida cad)
                        main
              "2" -> do putStrLn "DECODIFICAR. Introduzca cadena a decodificar."
                        cad <- getLine
                        putStrLn (cadenaExpandida cad)
                        main
              "3" -> return ()
