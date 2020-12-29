--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.List

-- KOLAKOSKI
contadora :: (Eq a) => [a] -> [Int]
contadora xs = map length (group xs)

contada :: (Eq a) => [Int] -> [a] -> [a]
contada (n:ns) (x:xs) = replicate n x ++ contada ns (xs++[x])
contada [] _ = []

sucKolakoski :: [Int]
sucKolakoski = 1 : 2 : xs
    where xs = 2 : contada xs [1,2]

kolakoski :: Int -> Int
kolakoski n = sucKolakoski !! (n-1)

-- GOLOMB
sucGolom :: [Int]
sucGolom = 1 : 2 : 2 : concat [replicate x i | (i, x) <- zip [3..] (drop 2 sucGolom)]

golom :: Int -> Int
golom n = sucGolom !! (n-1)