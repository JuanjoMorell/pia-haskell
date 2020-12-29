--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 6
module ListaPar (ListaP2,comprimida,expandida) where

-- EJERCICIO 1 
data ListaP a = LP [(Int,a)]

-- EJERCICIO 2
instance (Show a) => Show (ListaP a) where
    show (LP []) = ""
    show (LP ((i,e):p)) = show i ++ "->" ++ show e ++ "\n"

-- EJERCICIO 3
type ListaP2 a = [(Int,a)]

-- EJERCICIO 4
comprimida :: Eq a => [a] -> ListaP2 a
comprimida [] = []
comprimida (x:xs) = (1 + (length(takeWhile (==x) xs)), x) : comprimida(dropWhile (==x) xs)

-- EJERCICIO 5
insertar :: a -> Int -> [a]
insertar a 0 = []
insertar a i = [a] ++ insertar a (i-1)

expandida :: Eq a => ListaP2 a -> [a]
expandida xs = concat [insertar a i | (i,a) <- xs]

