--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Char

------------------------------------
------------------------- APARTADO A
------------------------------------

-- EJERCICIO 1
type Simbolo = Char
type Estado = Int
type Alfabeto = [Simbolo]
type Cinta = [Simbolo]

data Accion = L | R | SSK | Ninguna
    deriving(Eq,Show)

-- EJERCICIO 2
type Cuadruplas = [(Estado,Simbolo,Accion,Estado)]

delta :: Cuadruplas -> (Estado,Simbolo) -> (Accion,Estado)
delta cuad (e,s) = head[(accion,estado) | (estado,simbolo,accion,estado2) <- cuad, estado == e, simbolo == s]

data MT = MT (Alfabeto, Cuadruplas)

-- EJERCICIO 3
data Configuracion = C Estado Simbolo Cinta Cinta

miConfig :: Configuracion
miConfig = C 1 'a' "" "bcd"

espacios :: Int -> String
espacios 0 = " "
espacios n = " " ++ espacios (n-1)

instance Show Configuracion where
    show (C estado simbolo "" cinta2) = [simbolo] ++ cinta2 ++ ['\n'] ++  "^" ++ ['\n'] ++ show estado
        
        --show cinta1 ++ show simbolo ++ show cinta2 ++ "/n" ++ espacios cinta1 ++ "^" ++ "/n" ++ show estado