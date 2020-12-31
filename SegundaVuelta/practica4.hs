--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 1
ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [_] = True
ordenada (x:y:xs) = x <= y && ordenada (y:xs)

-- EJERCICIO 2
borrar :: Eq a => a -> [a] -> [a]
borrar c [] = []
borrar c (x:xs) = if x == c then xs else x:(borrar c xs)

-- EJERCICIO 3
insertar :: Ord a => a -> [a] -> [a]
insertar c [] = [c]
insertar c (x:xs) = if c <= x then c:x:xs else x:(insertar c xs)

-- EJERCICIO 4
ordInsercion :: (Ord a) => [a] -> [a]
ordInsercion [] = []
ordInsercion (x:xs) = insertar x (ordInsercion xs)

-- EJERCICIO 5
minimo :: (Ord a) => [a] -> a
minimo = head . ordInsercion

-- EJERCICIO 6
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] [] = []
mezcla [] xs = ordInsercion xs
mezcla (x:xs) ys = insertar x (mezcla xs ys)

-- EJERCICIO 7
mitades :: [a] -> ([a],[a])
mitades [] = ([],[])
mitades xs = (take n xs, drop n xs)
    where n = length xs `div` 2
    
-- EJERCICIO 8
ordMezcla :: Ord a => [a] -> [a]
ordMezcla [] = []
ordMezcla [x] = [x]
ordMezcla xs = mezcla (ordMezcla x) (ordMezcla y)
            where (x,y) = mitades xs

-- EJERCICIO 9
esPermutacion :: Eq a => [a] -> [a] -> Bool
esPermutacion [] [] = True
esPermutacion [] ys = True
esPermutacion (x:xs) ys = elem x ys && esPermutacion xs (borrar x ys)

-- EJERCICIO 1
cAplica :: (a -> [b]) -> [a] -> [b]
cAplica f xs = concat (map f xs)

-- EJERCICIO 2
rotaLista :: [a] -> [[a]]
rotaLista xs = rota xs (length xs)
    where
        rota _ 0 = []
        rota (x:xs) n = (x:xs) : rota (xs ++ [x]) (n-1)

-- EJERCICIO 3
quitarCabeza :: (Eq a) => [a] -> [[a]] -> [[a]]
quitarCabeza xs xss = filter buena xss
    where
        buena [] = False
        buena (w:ws) = w `notElem` xs