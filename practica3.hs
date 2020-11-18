--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-} 

module Numeros where

--------------------RACIONALES----------------------
----------------------------------------------------
-----------------------------------------EJERCICIO 1 
----------------------------------------------------

-- Se define el tipo de dato mediante renombramiento de dos enteros
-- (NUMERADOR, DENOMINADOR)
type Racional = (Integer,Integer)

-- Funcion simplificar
-- Representación canónica del racional
simplificar (n,d) = (((signum d)*n) `div` m, (abs d) `div` m)
    where m = gcd n d

-- Funcion multiplicar
multiplicar :: Racional -> Racional -> Racional
multiplicar (n1,d1) (n2, d2) = simplificar (n1*n2, d1*d2)

-- Funcion dividir
dividir :: Racional -> Racional -> Racional
dividir (n1,d1) (n2, d2) = simplificar(n1*d2, d1*n2)

-- Funcion sumar
sumar :: Racional -> Racional -> Racional
sumar (n1,d1) (n2, d2) = simplificar((n1*d2+n2*d1), d1*d2)

-- Funcion restar
restar :: Racional -> Racional -> Racional
restar (n1,d1) (n2, d2) = simplificar((n1*d2-n2*d1), d1*d2)

muestraRac :: Racional -> String
muestraRac (n,d)
    | dS == 1 = show nS
    | otherwise =  show nS ++ "/" ++ show dS
        where (nS, dS) = simplificar(n,d)

----------------------------------------------------
-----------------------------------------EJERCICIO 2 
----------------------------------------------------
data Racional2 = Rac Integer Integer
    deriving (Eq)

simplificar2 (Rac n d) = (Rac (((signum d)*n) `div` m) ((abs d) `div` m))
    where m = gcd n d

-- Funcion multiplicar
multiplicar2 :: Racional2 -> Racional2 -> Racional2
multiplicar2 (Rac n1 d1) (Rac n2 d2) = simplificar2 (Rac (n1*n2) (d1*d2))

-- Funcion dividir
dividir2 :: Racional2 -> Racional2 -> Racional2
dividir2 (Rac n1 d1) (Rac n2 d2) = simplificar2 (Rac (n1*d2) (d1*n2))

-- Funcion sumar
sumar2 :: Racional2 -> Racional2 -> Racional2
sumar2 (Rac n1 d1) (Rac n2 d2) = simplificar2 (Rac (n1*d2+n2*d1) (d1*d2))

-- Funcion restar
restar2 :: Racional2 -> Racional2 -> Racional2
restar2 (Rac n1 d1) (Rac n2 d2) = simplificar2 (Rac (n1*d2-n2*d1) (d1*d2))

instance Show Racional2 where
    show (Rac n 1) = show n
    show (Rac n d) = show nS ++ "/" ++ show dS
        where (Rac nS dS) = simplificar2 (Rac n d)

----------------------------------------------------
-----------------------------------------EJERCICIO 3 
----------------------------------------------------

instance Num Racional2 where
    (*) = multiplicar2
    (+) = sumar2
    (-) = restar2
    negate (Rac n d) = simplificar2 (Rac (-n) d)
    fromInteger x = (Rac x 1)
    signum (Rac n d) = if (n*d) < 0 then -1 else 1
    abs (Rac n d) = if (n*d)<0 then simplificar2(Rac (-n) d) else simplificar2(Rac n d)


--------------------NATURALES-----------------------
----------------------------------------------------
-----------------------------------------EJERCICIO 1 
----------------------------------------------------

