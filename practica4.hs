--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-} 

----------------------------------------------------
-----------------------------------------EJERCICIO 1
----------------------------------------------------
ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [_] = True
ordenada (x:y:xs) = x <= y && ordenada(xs)

----------------------------------------------------
-----------------------------------------EJERCICIO 2
----------------------------------------------------
borrar :: Eq a => a -> [a] -> [a]
borrar x [] = []
borrar x (y:ys) | x == y = ys
                | otherwise = y : borrar x ys

----------------------------------------------------
-----------------------------------------EJERCICIO 3
----------------------------------------------------
insertar :: Ord a => a -> [a] -> [a]
insertar x [] = [x]
insertar x (y:ys) | x <= y = (x:y:ys)
                  | otherwise = y : insertar x ys

----------------------------------------------------
-----------------------------------------EJERCICIO 4
----------------------------------------------------
ordInsercion :: Ord a => [a] -> [a]
ordInsercion [] = []
ordInsercion (x:xs) = insertar x (ordInsercion xs)

----------------------------------------------------
-----------------------------------------EJERCICIO 5
----------------------------------------------------
minimo :: Ord a => [a] -> a
minimo = head . ordInsercion

----------------------------------------------------
-----------------------------------------EJERCICIO 6
----------------------------------------------------
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] [] = []
mezcla xs [] = ordInsercion xs
mezcla [] ys = ordInsercion ys
mezcla (x:xs) (y:ys) | x <= y = x : mezcla xs (y:ys)
                     | otherwise = y : mezcla (x:xs) ys

----------------------------------------------------
-----------------------------------------EJERCICIO 7
----------------------------------------------------
mitades :: [a] -> ([a],[a])
mitades xs = (take l xs, drop l xs)
    where l = length xs `div` 2

----------------------------------------------------
-----------------------------------------EJERCICIO 8
----------------------------------------------------
ordMezcla :: Ord a => [a] -> [a]
ordMezcla [] = []
ordMezcla [x] = [x]
ordMezcla xs = mezcla (ordMezcla ys) (ordMezcla zs)
                where (ys, zs) = mitades xs

----------------------------------------------------
-----------------------------------------EJERCICIO 9
----------------------------------------------------
esPermutacion :: Eq a => [a] -> [a] -> Bool
esPermutacion [] [] = True
esPermutacion [] (y:ys) = False
esPermutacion (x:xs) ys = elem x ys && esPermutacion xs (borrar x ys)