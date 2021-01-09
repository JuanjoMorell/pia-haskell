--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Rosadelfa where

data RAdelfa a = Nodo a [RAdelfa a]

-- EJERCICIO 1
instance (Show a)=> Show (RAdelfa a) where
	show (Nodo d xs) = shown 0 (Nodo d xs)

midelfa = Nodo 1 [Nodo 2 [Nodo 3 []], Nodo 4 []]

espacios :: Int -> String
espacios 0 = ""
espacios n = " " ++ espacios (n-1)

shown :: (Show a) => Int -> RAdelfa a -> String  
shown n (Nodo a []) = espacios n ++ show a ++ "\n"
shown n (Nodo a xs) = espacios n ++ show a ++ "\n" ++ concat (map ((++"\n").(shown (n+1)))  xs)

-- EJERCICIO 2
podar :: RAdelfa a -> RAdelfa a
podar (Nodo a xs) = Nodo a (tail xs)

-- EJERCICIO 3
aplanar :: RAdelfa a -> [a]
aplanar (Nodo a xs) = a:(concat (map aplanar xs))