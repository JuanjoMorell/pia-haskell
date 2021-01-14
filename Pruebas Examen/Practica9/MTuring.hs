--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

module MTuring where

import Data.Char

------------------------------------
------------------------- APARTADO A
------------------------------------

-- EJERCICIO 1
type Simbolo = Char
type Estado = Int
type Alfabeto = [Simbolo]
type Cinta = [Simbolo]

data Accion = L | R | S Simbolo | Ninguna
    deriving(Eq,Show)

-- EJERCICIO 2
type Cuadruplas = [(Estado,Simbolo,Accion,Estado)]

delta :: Cuadruplas -> (Estado,Simbolo) -> (Accion,Estado)
delta [] (e,s) = (Ninguna, 0)
delta ((est,sim,acc,est2):cuad) (e,s) = if (e == est && sim == s) then (acc,est2) else (delta cuad (e,s))

data MT = MT (Alfabeto, Cuadruplas)

-- EJERCICIO 3
data Configuracion = C Estado Simbolo Cinta Cinta

espacios :: Int -> String
espacios 0 = " "
espacios n = " " ++ espacios (n-1)

instance Show Configuracion where
    show (C estado simbolo "" cinta2) = [simbolo] ++ cinta2 ++ "\n" ++ ['^'] ++ "\n" ++ show estado ++ "\n"
    show (C estado simbolo cinta1 cinta2) = cinta1 ++ [simbolo] ++ cinta2 ++ "\n" ++ espacios((length cinta1) - 1) ++ "^" ++ "\n" ++espacios((length cinta1) - 1)++ show estado ++"\n"

-- EJERCICIO 4
actualizaCinta :: Accion -> (Cinta,Simbolo,Cinta) -> (Cinta,Simbolo,Cinta)
actualizaCinta L (c1,s,c2) = if (c1 == []) then (c1, ' ', s:c2) else if s == ' ' then (reverse(tail c1'), head c1', c2) else (reverse(tail c1'), head c1', s:c2)
    where c1' = reverse c1
actualizaCinta R (c1,s,c2) = if (c2 == []) then (s:c1, ' ', c2) else if s == ' ' then (c1, head c2, tail c2) else (c1++[s], head c2, tail c2)
actualizaCinta (S sim) (c1,s,c2) = (c1,sim,c2)
actualizaCinta Ninguna (c1,s,c2) = error "NO HAY ACCION"

-- EJERCICIO 5
pasosCalculo :: MT -> Configuracion -> [Configuracion]
pasosCalculo (MT (alf,cuad)) (C est sim c1 c2) = if (ac,est2) == (Ninguna,0) then [(C est sim c1 c2)] else [(C est sim c1 c2)] ++ (pasosCalculo (MT (alf,cuad)) (C est2 simAct c1Act c2Act))
    where (ac, est2) = delta cuad (est,sim)
          (c1Act, simAct, c2Act) = actualizaCinta ac (c1,sim,c2)

-- EJERCICIO 6
calculo :: MT -> String -> [Configuracion]
calculo mt st = pasosCalculo mt (C 1 ' ' "" st)

-- EJERCICIO 7
calcula :: MT -> String -> Cinta
calcula mt st = c1 ++ [sim] ++ c2
    where (C est sim c1 c2) = last (calculo mt st)
