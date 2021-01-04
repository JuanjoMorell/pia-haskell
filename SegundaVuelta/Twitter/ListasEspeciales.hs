--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 6
module ListasEspeciales(tamMax, ListaEspecial, recortarLista, esCorta, esLarga) where

-- EJERCICIO 1
tamMax :: Int
tamMax = 280

type ListaEspecial a = [a]

-- EJERCICIO 2
esCorta, esLarga :: (Ord a) => ListaEspecial a -> Bool
esCorta ls = length ls <= tamMax
esLarga = not . esCorta

-- EJERCICIO 3
comienzaPor, terminaPor :: (Eq a) => a -> ListaEspecial a -> Bool
comienzaPor x ls = x == (head ls)
terminaPor x ls = x == (head (reverse ls))

-- EJERCICIO 4
todosIguales :: (Eq a) => ListaEspecial a -> Bool
todosIguales ls = (length [x | x <- ls, x == elem]) == (length ls)
    where elem = head ls

-- EJERCICIO 5
recortarLista :: (Eq a) => ListaEspecial a -> ListaEspecial a
recortarLista ls = if length ls > tamMax then (take tamMax ls) else ls
