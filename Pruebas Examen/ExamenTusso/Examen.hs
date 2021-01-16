{-# OPTIONS_GHC -fno-warn-tabs #-}

{-# LANGUAGE NPlusKPatterns #-}

import Data.List
import Data.Char

-- EJERCICIO 1
type Palabra = String
type Linea = [Palabra]

-- EJERCICIO 2


-- EJERCICIO 6
take' :: Int -> String -> String -> Linea
take' n [] ac = [ac]
take' 0 st ac = [ac]
take' n st ac
    | (head st) == ' ' =  take' (n-1) (drop 1 st) (ac++[(head st)])
    | otherwise = if (length palabraActual) <= n then take' (n-(length palabraActual)) (drop (length palabraActual) st) (ac++palabraActual) else take' 0 st ac
    where palabraActual = head(words st)

contarLinea :: Linea -> Int
contarLinea ln = sum [length x | x <- ln]

separaEnLineas :: String -> [Linea]
separaEnLineas [] = []
separaEnLineas st = [lineaAct] ++ separaEnLineas (drop (contarLinea lineaAct) st)
    where lineaAct = take' 80 st []

-- EJERCICIO 7
espacios :: Int -> String
espacios 0 = ""
espacios n = " " ++ espacios (n-1)

justifica :: String -> String
justifica st = (concat [x ++ espacios nEsp | x <- (init palabras)]) ++ last palabras
    where palabras = words st
          longitud = length st
          espaciosAct = (length palabras) - 2
          espaciosFaltan = 80 - longitud
          nEsp = (espaciosAct+espaciosFaltan) `div` ((length palabras)-1)

-- EJERCICIO 8
lineaAstring :: Linea -> String
lineaAstring ln = concat [x | x <- ln]

juntarLineas :: [Linea] -> String
juntarLineas lns = concat [unlines x | x <- lns]

-- EJERCICIO 9
main :: IO()
main = do putStr "Introduce nombre de fichero de entrada "
          fentrada <- getLine
          putStr "Introduce nombre de fichero de salida "
          fsalida <- getLine
          contenido <- readFile fentrada
          let lineas = separaEnLineas contenido
          let justificadas = juntarLineas lineas
          writeFile fsalida justificadas
          putStrLn "FICHERO JUSTIFICADO, SALUDOS"