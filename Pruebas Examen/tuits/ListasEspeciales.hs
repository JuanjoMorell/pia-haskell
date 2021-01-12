--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 6
module ListasEspeciales where

import Data.Char
import Data.List

-- EJERCICIO 1
tamMax :: Int
tamMax = 280

type ListaEspecial a = [a]

-- EJERCICIO 2
esCorta, esLarga :: ListaEspecial a -> Bool
esCorta xs = length xs <= tamMax
esLarga xs = length xs > tamMax

-- EJERCICIO 3
comienzaPor, terminaPor :: (Eq a) => a -> ListaEspecial a -> Bool
comienzaPor c xs = c == (head xs)
terminaPor c xs = c == (head (reverse xs))

-- EJERCICIO 4
todosIguales :: Eq a => ListaEspecial a -> Bool
todosIguales xs = length (takeWhile (== x) xs) == length xs
    where x = head xs

-- EJERCICIO 5
recortaLista :: ListaEspecial a -> ListaEspecial a
recortaLista xs = if esCorta xs then xs else (take tamMax xs)
