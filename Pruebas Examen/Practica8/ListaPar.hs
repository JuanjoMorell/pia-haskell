--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 6
module ListaPar (ListaP2, comprimida, expandida) where

-- EJERCICIO 1
data ListaP a = LP [(Int,a)]

-- EJERCICIO 2
instance (Show a) => Show (ListaP a) where
	show (LP []) =""
	show (LP ((i,e):ps)) = show i ++ "->" ++ show e ++ "\n" ++ show (LP ps)

-- EJERCICIO 3
type ListaP2 a = [(Int,a)]

-- EJERCICIO 4
comprimida :: (Eq a) => [a] -> ListaP2 a
comprimida [] = []
comprimida xs = [(length(takeWhile (== primero) xs), head xs)] ++ (comprimida (dropWhile (== primero) xs))
    where primero = head xs

-- EJERCICIO 5
expandida :: (Eq a) => ListaP2 a -> [a]
expandida [] = []
expandida ((i,a):xs) = replicate i a ++ expandida xs