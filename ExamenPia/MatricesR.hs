--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 4
module MatricesR(Matriz, productoCartesianoF, combinaMatrices) where

import Data.List
import Data.Char

-- EJERCICIO 1
type Matriz a = [[a]]

-- EJERCICIO 2
productoCartesianoF :: (a -> a -> a) -> [a] -> [a] -> Matriz a
productoCartesianoF f [] ys = []
productoCartesianoF f (x:xs) ys = [f x y | y <- ys] : (productoCartesianoF f xs ys)

-- EJERCICIO 3
obtenerTraspuesta :: Matriz a -> Matriz a
obtenerTraspuesta ma = transpose ma

aplicarFuncion :: (a -> a -> a) -> [a] -> [a] -> [a]
aplicarFuncion f xs ys = [f x y | (x,y) <- zip xs ys]

aplicarFuncion2 :: (Ord a) => (a -> a -> a) -> [a] -> a
aplicarFuncion2 f xs = foldr f (head xs) xs

combinaMatrices :: (Ord a) => (a -> a -> a) -> (a -> a -> a) -> [a] -> Matriz a -> [a]
combinaMatrices f g xs ma = [aplicarFuncion2 f x | x <- mFuncionG]
    where mFuncionG = [aplicarFuncion g xs x | x <- (obtenerTraspuesta ma)]
