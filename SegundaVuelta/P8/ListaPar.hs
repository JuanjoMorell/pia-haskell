--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 6
module ListaPar where

-- EJERCICIO 1
data ListaP a = LP [(Int,a)]

-- EJERCICIO 2
instance (Show a) => Show (ListaP a) where
    show (LP []) = ""
    show (LP ((i,e):ls)) = show i ++ " -> " ++ show e ++ "\n" ++ show (LP ls)

-- EJERCICIO 3
type ListaP2 a = [(Int,a)]

-- EJERCICIO 4
comprimida :: Eq a => [a] -> ListaP2 a
comprimida [] = []
comprimida (x:xs) = (1+length (takeWhile (==x) xs), x) : (comprimida (dropWhile (==x) xs))

-- EJERCICIO 5
expandida :: ListaP2 a -> [a]
expandida [] = []
expandida ((i,e):ps) = [e | x <- [1..i]] ++ (expandida ps)