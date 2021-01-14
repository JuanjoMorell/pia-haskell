--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import MTuring

-- EJERCICIO 1
aceptaPalabra :: MT -> IO()
aceptaPalabra mt = do putStr "Introduzca cadena: "
                      cad <- getLine
                      putStr "Quiere ver el calculo completo? (Y/N) "
                      opcion <- getLine
                      if opcion == "Y" then putStrLn (concat(map show (calculo mt cad))) else putStrLn (calcula mt cad)

-- EJERCICIO 2
leerSim :: String -> Simbolo
leerSim sim = if sim == "#" then ' ' else (head sim)

leerAcc :: String -> Accion
leerAcc acc
    | length(acc') == 1 = if acc' == "d" then R else L
    | length(acc') == 2 = (S (leerSim (tail acc')))
    | otherwise = Ninguna
    where acc' = tail acc

leerCuadruplas :: String -> Cuadruplas
leerCuadruplas st = [(read est, leerSim sim , leerAcc acc, read est2) | [est, sim, acc, est2] <- cuadrupla]
    where lineas = lines st
          cuadrupla = [words l | l <- lineas]

main :: IO()
main = do putStr "Introduzca fichero de la Maquina de Turing: "
          file <- getLine
          contenido <- readFile file
          let cuad = leerCuadruplas contenido
          putStr "Introduzca alfabeto de la Maquina de Turing: "
          alf <- getLine
          let mt = MT (alf, cuad)
          aceptaPalabra mt
          putStrLn "PALABRA ACEPTADA"