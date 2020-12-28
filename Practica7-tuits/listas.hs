--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Listas (tamMax,ListaEspecial,esCorta,esLarga, recortaLista) where

-- EJERCICIO 1
tamMax :: Int
tamMax = 280

type ListaEspecial a = [a]

-- EJERCICIO 2
esCorta, esLarga :: ListaEspecial a -> Bool
esCorta lista = length lista <= tamMax
esLarga lista = length lista > tamMax

-- EJERCICIO 3
comienzaPor, teminarPor :: (Eq a) => a -> ListaEspecial a -> Bool
comienzaPor a lista = head lista == a
teminarPor a lista = last lista == a

-- EJERCICIO 4
todosIguales :: (Eq a) => ListaEspecial a -> Bool
todosIguales xs = length xs == length [x | x <- xs, x == elem]
    where elem = head xs

-- EJERCICIO 5
recortaLista :: ListaEspecial a -> ListaEspecial a
recortaLista xs = if esLarga xs then take tamMax xs else xs