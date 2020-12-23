--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import ImagenesSVG

-- EJERCICIO 1
cuatroImg :: Imagen -> Imagen
cuatroImg pic = left `junto_a` (giraV right)
    where left = pic `encima` (invierte_color pic)
          right = ((invierte_color pic) `encima` pic) 

-- EJERCICIO 2
negroBlanco :: Integer -> Imagen
negroBlanco n
            | n <= 1 = negro
            | otherwise = negro `junto_a` blancoNegro(n-1)

blancoNegro :: Integer -> Imagen
blancoNegro n
            | n <= 1 = blanco
            | otherwise = blanco `junto_a` negroBlanco(n-1)

ajedrezNegro :: Integer -> Integer -> Imagen
ajedrezNegro m n
            | n <= 1 = negroBlanco m
            | otherwise = negroBlanco m `encima` ajedrezBlanco m (n-1)

ajedrezBlanco :: Integer -> Integer -> Imagen
ajedrezBlanco m n
            | n <= 1 = blancoNegro m
            | otherwise = blancoNegro m `encima` ajedrezNegro m (n-1)

ajedrez :: Integer -> Integer -> Imagen
ajedrez n m = ajedrezBlanco n m

-- EJERCICIO 3
blanco' = caballoPequeño
negro' = invierte_color (giraV caballoPequeño)

negroBlanco' :: Integer -> Integer -> Imagen
negroBlanco' n m
            | n <= 1 || m == (8-n+1) = negro' 
            | n <= 1 = negro
            | m == (8-n+1) = negro' `junto_a` blancoNegro'(n-1) m
            | otherwise = negro `junto_a` blancoNegro'(n-1) m

blancoNegro' :: Integer -> Integer -> Imagen
blancoNegro' n m 
            | n == m && n <= 1 = blanco'
            | n <= 1 = blanco
            | n == m = blanco' `junto_a` negroBlanco'(n-1) m
            | otherwise = blanco `junto_a` negroBlanco'(n-1) m

ajedrezNegro' :: Integer -> Integer -> Imagen
ajedrezNegro' m n
            | n <= 1 = negroBlanco' m n
            | otherwise = negroBlanco' m n `encima` ajedrezBlanco' m (n-1)

ajedrezBlanco' :: Integer -> Integer -> Imagen
ajedrezBlanco' m n
            | n <= 1 = blancoNegro' m n
            | otherwise = blancoNegro' m n `encima` ajedrezNegro' m (n-1)

ajedrez' :: Integer -> Integer -> Imagen
ajedrez' n m = ajedrezBlanco' n m