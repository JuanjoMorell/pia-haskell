--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Char
import System.IO
import Data.List ((\\))

main20 = putStr "Me llamo " >> putStr"juan " >> putStrLn "jose"

mainDo = do putStr "Me llamo "
            putStr "juan "
            putStrLn "jose"

main20' = getLine >>= \cad -> putStrLn cad

mainDo' = do cad <- getLine
             let cad2 = map toUpper cad
             putStrLn cad2

-- EJERCICIO 1
revelar = do putStr "Introduzca un caracter -> "
             c <- getChar
             putStr "El caracter convertido es: "
             putStrLn (show(fromEnum c))

-- EJERCICIO 2
palindromo = do putStr "Introduzca una cadena -> "
                cad <- getLine
                if cad == reverse cad then putStrLn "Es palindromo" else putStrLn "No es palindromo"

-- EJERCICIO 3
leerPalabra :: IO String
leerPalabra = do c <- getChar
                 if c == '\n'
                     then do putChar c
                             return []
                     else do putChar '-'
                             cs <- leerPalabra
                             return (c:cs)

comparar :: String -> String -> String
comparar s t = map comprobar s
    where comprobar c = if elem c t then c else '-'

adivinar :: String -> IO ()
adivinar s = do putStr "Introduzca una palabra -> "
                intento <- getLine
                if s == intento 
                    then putStrLn "Palabra correcta!!" 
                    else do putStrLn (comparar s intento)
                            adivinar s

ahorcado :: IO ()
ahorcado = do putStrLn "Introduzca una palabra -> "
              cad <- leerPalabra
              putStrLn "Adivina la palabra"
              adivinar cad

-- EJERCICIO 4
eliminarAscii :: IO()
eliminarAscii = do putStrLn "Introduzca direccion fichero de entrada -> "
                   fichero <- getLine
                   putStrLn "Introduzca direccion fichero de salida -> "
                   ficheroSalida <- getLine
                   entrada <- readFile fichero
                   writeFile ficheroSalida (filter isAscii entrada)
                   putStrLn "Filtrado finalizado"

-- EJERCICIO 5
type Tablero = [Int]

reinas :: Int -> [Tablero]
reinas n = reinasAux n
	   where reinasAux 0 = [[]]
	         reinasAux (m+1) =[r:rs | rs <- reinasAux m,
				       r <- ([1..n] \\ rs),
                                       noAtaca r rs 1] 

noAtaca :: Int -> Tablero -> Int -> Bool 
noAtaca _ [] _ = True
noAtaca r (a:rs) distH = abs(r-a) /= distH &&
			 noAtaca r rs (distH + 1)

main = do putStr "Introduzca numero de reinas -> "
          nreinas <- getInt
          dibujaTableros (reinas nreinas)

dibujaTableros:: [Tablero] -> IO()
dibujaTableros [] = putStr ""
dibujaTableros (x:xs) = dibujaTablero x (length x) >>putStrLn "">> dibujaTableros xs 

dibujaTablero :: Tablero -> Int -> IO ()
dibujaTablero xs 0 = putStr ""
dibujaTablero xs n = putStrLn (hazLinea xs (length xs) n)  >> dibujaTablero xs (n-1) 

hazLinea :: [Int] -> Int -> Int -> String
hazLinea [] l _ = ""
hazLinea (x:xs) l n = if x==(l-n+1) then ('R' : hazLinea xs l n) else ('X' : hazLinea xs l n)