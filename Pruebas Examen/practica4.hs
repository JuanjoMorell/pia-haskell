--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Practica4 where

-- EJERCICIO 1
ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [_] = True
ordenada (x:y:xs) = x <= y && ordenada (y:xs)

-- EJERCICIO 2
borrar :: Eq a => a -> [a] -> [a]
borrar a [] = []
borrar a (x:xs) = if a == x then xs else x:(borrar a xs)

-- EJERCICIO 3
insertar :: Ord a => a -> [a] -> [a]
insertar a [] = [a]
insertar a (x:xs) = if a <= x then (a:x:xs) else x:(insertar a xs)

-- EJERCICIO 4
ordInsercion :: Ord a => [a] -> [a]
ordInsercion [] = []
ordInsercion (x:xs) = insertar x (ordInsercion xs)

-- EJERCICIO 5
minimo :: Ord a => [a] -> a
minimo = head . ordInsercion

-- EJERCICIO 6
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] ys = ys
mezcla (x:xs) ys = insertar x (mezcla xs ys')
    where ys' = ordInsercion ys

-- EJERCICIO 7
mitades :: [a] -> ([a],[a])
mitades xs = (take mitad xs, drop mitad xs)
    where mitad = (length xs) `div` 2

-- EJERCICIO 8
ordMezcla :: Ord a => [a] -> [a]
ordMezcla [] = []
ordMezcla [x] = [x]
ordMezcla xs = mezcla (ordMezcla m1) (ordMezcla m2)
    where (m1,m2) = mitades xs

-- EJERCICIO 9
esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion xs ys = (ordMezcla xs) == (ordMezcla ys)