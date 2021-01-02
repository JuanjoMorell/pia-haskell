--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import System.IO
import Data.Char

-- EJERCICIO 1
revelar :: IO ()
revelar = do putStr "Introduzca un caracter: "
             c <- getChar
             let convert = fromEnum(c)
             putStrLn (show convert)

-- EJERCICIO 2
comprobarPalin :: String -> Bool
comprobarPalin xs = xs' == (reverse xs')
    where xs' = map toLower xs

palindromo :: IO ()
palindromo = do putStr "Introduzca una palabra: "
                cad <- getLine
                if comprobarPalin cad
                    then putStrLn "SI"
                    else putStrLn "NO"

-- EJERCICIO 3
mostrarPalabra :: String -> String
mostrarPalabra xs = concat ["_" | x <- xs]

adivinarPalabra :: String -> String -> String
adivinarPalabra xs [] = concat ["_" | x <- xs]
adivinarPalabra [] ys = []
adivinarPalabra (x:xs) (y:ys) = if x == y then [y] ++ adivinarPalabra xs ys else "_" ++ adivinarPalabra xs ys

combinarIntentos :: String -> String -> String
combinarIntentos [] [] = []
combinarIntentos [] ys = []
combinarIntentos xs [] = []
combinarIntentos (x:xs) (y:ys)
                | x == y = [x] ++ combinarIntentos xs ys
                | not (isLetter x) && isLetter y = [y] ++ combinarIntentos xs ys
                | isLetter x && not (isLetter y) = [x] ++ combinarIntentos xs ys
                | otherwise = "_" ++ combinarIntentos xs ys

compIntento :: String -> Bool
compIntento [] = True
compIntento (x:xs) = isLetter x && compIntento xs

adivinar :: String -> String -> IO ()
adivinar cad int = do putStr "Intete adivinar la palabra: "
                      hint <- getLine
                      let intento = adivinarPalabra cad hint
                      let cadena = combinarIntentos intento int
                      putStrLn cadena
                      if compIntento intento
                          then putStrLn "Has acertado!"
                          else adivinar cad cadena

ahorcado = do putStr "Introduzca palabra a adivinar: "
              cad <- getLine
              putStrLn (mostrarPalabra cad)
              adivinar cad (mostrarPalabra cad)

-- EJERCICIO 4
filtrarAscii = do putStr "Introduzca fichero de entrada: "
                  fEntrada <- getLine
                  putStr "Introduzca fichero de salida: "
                  fSalida <- getLine
                  contenido <- readFile fEntrada
                  writeFile fSalida (filter isAscii contenido)
              

