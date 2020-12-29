--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import ListaPar
import Data.Char
import Data.List

-- EJERCICIO 1
type ListaPCh = ListaP2 Char

-- EJERCICIO 2
listaAcadena :: ListaPCh -> String
listaAcadena xs = concat [mostrar i a | (i,a) <- xs] 
    where mostrar i a = show i ++ [a]

-- EJERCICIO 3
cadenaComprimida :: String -> String
cadenaComprimida xs = listaAcadena (comprimida xs)

-- EJERCICIO 4
cadenaAlista :: String -> ListaPCh
cadenaAlista [] = []
cadenaAlista cs = (read ns,x) : cadenaAlista xs
    where (ns,(x:xs)) = span isNumber cs  

-- EJERCICIO 5
cadenaExpandida :: String -> String
cadenaExpandida = expandida . cadenaAlista

-- EJERCICIO 6
main :: IO ()
main = do putStrLn "Bienvenido a codificacion/decodificacion RLE"
          putStrLn "(1) Codificar"
          putStrLn "(2) Descodificar"
          putStrLn "(3) Salir"
          opcion <- getLine
          case opcion of
              "1" -> do putStr "Introduzca cadena a Codificar: "
                        cad <- getLine
                        let cadCod = cadenaComprimida cad
                        putStr "Cadena codificada: "
                        putStrLn cadCod
                        main
              "2" -> do putStr "Introduzca cadena a Descodificar: "
                        cad <- getLine
                        let cadDescod = cadenaExpandida cad
                        putStr "Cadena descodificada: "
                        putStrLn cadDescod
                        main
              "3" -> return()
