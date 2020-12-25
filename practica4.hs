--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

---------------------------------------------------------------
---------------------- PARTE 1 --------------------------------
---------------------------------------------------------------

-- EJERCICIO 1
andList :: [Bool] -> Bool
andList [] = True
andList (x:xs) = x && andList xs

ordenada :: Ord a => [a] -> Bool
ordenada xs = andList(map mayor (zip xs (tail xs)))
        where mayor (a,b) = a <= b

ordenada' :: Ord a => [a] -> Bool
ordenada' [] = True
ordenada' [_] = True
ordenada' (x:y:xs) = x <= y && ordenada' (y:xs)

-- EJERCICIO 2
borrar :: Eq a => a -> [a] -> [a]
borrar a [] = []
borrar a (x:xs) = if x == a then borrar a xs else x : borrar a xs

-- EJERCICIO 3
insertar :: Ord a => a -> [a] -> [a]
insertar a [] = [a]
insertar a (x:xs) = if a <= x then a:x:xs else x: insertar a xs

-- EJERCICIO 4
ordInsercion :: Ord a => [a] -> [a]
ordInsercion [] = []
ordInsercion (x:xs) = insertar x (ordInsercion xs)

ordInsercion' :: Ord a => [a] -> [a]
ordInsercion' = foldr insertar [] 

-- EJERCICIO 5
minimo :: Ord a => [a] -> a
minimo = head . ordInsercion

-- EJERCICIO 6
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] [] = []
mezcla [] ys = ordInsercion ys
mezcla (x:xs) ys = mezcla xs (insertar x ys)

-- EJERCICIO 7
mitades :: [a] -> ([a],[a])
mitades xs = (take l xs, drop l xs)
    where l = length xs `div` 2

-- EJERCICIO 8
ordMezcla :: Ord a => [a] -> [a]
ordMezcla [] = []
ordMezcla [x] = [x]
ordMezcla xs = mezcla (ordMezcla x1) (ordMezcla x2)
    where (x1, x2) = mitades xs

-- EJERCICIO 9
esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion xs ys = (ordMezcla xs) == (ordMezcla ys)

---------------------------------------------------------------
---------------------- PARTE 2 --------------------------------
---------------------------------------------------------------

-- EJERCICIO 1
cAplica :: Ord a => (a -> [a]) -> [a] -> [a]
cAplica f xs = concat (map f xs)

-- EJERCICIO 2
rotaLista :: Ord a => [a] -> [[a]]
rotaLista xs = rota xs (length xs)
	where
		rota  _ 0 = []
		rota (x:xs) (n+1) = (x:xs) : rota (xs++[x]) n

-- EJERCICIO 3
quitaCabeza :: Ord a => [a] -> [[a]] -> [[a]]
quitaCabeza xs xss = filter buena xss
    where
            buena [] = False
            buena (w:ws) = w `notElem` xs

-- EJERCICIO 4
triviales :: [String]
triviales = ["la","las","como","de","a","con","su","el","un","en"]

rotaTitulo :: String -> [String]
rotaTitulo = map unwords.quitaCabeza triviales.rotaLista.words

-- EJERCICIO 5
kwic :: [String] -> [String]
kwic = ordMezcla.cAplica rotaTitulo