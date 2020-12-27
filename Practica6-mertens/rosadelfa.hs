--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Rosadelfa where

data RAdelfa a = Nodo a [RAdelfa a]


espacios :: Int -> String
espacios 0 = ""
espacios n = " "++ espacios (n-1)

shown :: (Show a) => Int -> RAdelfa a -> String
shown n (Nodo d []) = espacios n ++ show d
shown n (Nodo d xs) = espacios n ++ show d++"\n"++ concat (map ((++"\n").(shown (n+1)))  xs)

podar :: RAdelfa a -> RAdelfa a
podar (Nodo a xs) = Nodo a (tail xs)

aplanar :: RAdelfa a -> [a]
aplanar (Nodo a xs) = [a] ++ concat (map aplanar xs)