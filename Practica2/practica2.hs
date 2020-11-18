--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-} 

import ImagenesSVG

---------------------------------------------
---------------------------------EJERCICIO 1
---------------------------------------------

cuatroImg :: Imagen -> Imagen
cuatroImg(img) = left `junto_a` right
    where
    left = img `encima` invierte_color(img)
    right = giraV(invierte_color(img)) `encima` giraV(img)

---------------------------------------------
---------------------------------EJERCICIO 2
---------------------------------------------

-- Filas que empiezan por una casilla en negro
negroBlanco :: Integer -> Imagen
negroBlanco(n)
    | n <= 1 = negro
    | otherwise = negro `junto_a` blancoNegro(n-1)

-- Filas que empiezan por una casilla en blanco
blancoNegro :: Integer -> Imagen
blancoNegro(n)
    | n <= 1 = blanco
    | otherwise = blanco `junto_a` negroBlanco(n-1)

ajedrezNegro :: Integer -> Integer -> Imagen
ajedrezNegro n m
    | n <= 1 = negroBlanco(m)
    | otherwise = negroBlanco(m) `encima` ajedrezBlanco (n-1) m

ajedrezBlanco :: Integer -> Integer -> Imagen
ajedrezBlanco n m
    | n <= 1 = blancoNegro(m)
    | otherwise = blancoNegro(m) `encima` ajedrezNegro (n-1) m

---------------------------------------------
---------------------------------EJERCICIO 3
---------------------------------------------

negro' = invierte_color(giraV caballoPequeño)

negroBlanco' :: Integer -> Integer -> Imagen
negroBlanco m n
    | m <= 1 = negro
    | m <= 1 && m==(8-n+1) = negro'
    | m==(8-n+1) = negro' `junto_a` blancoNegro' (m-1) n
    | otherwise = negro `junto_a` blancoNegro' (m-1) n