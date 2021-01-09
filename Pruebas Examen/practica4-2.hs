--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Practica4

-- EJERCICIO 1
cAplica :: (a -> [b]) -> [a] -> [b]
cAplica f xs = concat (map f xs)

-- EJERCICIO 2
rotaLista :: Ord a => [a] -> [[a]]
rotaLista xs = [drop x xs ++ take x xs | x <- [0..n]]
    where n = (length xs) - 1

-- EJERCICIO 3
quitaCabeza :: Eq a => [a] -> [[a]] -> [[a]]
quitaCabeza xs xss = [l | l <- xss, (length l) /= 0,not (elem (head l) xs)]

-- EJERCICIO 4
triviales :: [String]
triviales = ["la", "las", "como", "de", "a", "con", "su", "el", "un", "en"]

rotaTitulo :: String -> [String]
rotaTitulo = map unwords . quitaCabeza triviales . rotaLista . words

-- EJERCICIO 5
kwic :: [String] -> [String]
kwic = ordMezcla . cAplica rotaTitulo