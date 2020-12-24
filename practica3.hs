--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- RACIONALES 

-- EJERCICIO 1
type Racional = (Int,Int)

simplificar (n,d) = (((signum d)*n) `div` m, (abs d) `div`m)
	where m = gcd n d

multRac :: Racional -> Racional -> Racional
multRac (x1,y1) (x2,y2) = simplificar(x1*x2, y1*y2)

divRac :: Racional -> Racional -> Racional
divRac (x1,y1) (x2,y2) = simplificar(x1*y2, y1*x2)

sumRac :: Racional -> Racional -> Racional
sumRac (x1,y1) (x2,y2) = simplificar(x1*y2 + x2*y1, y1*y2)

resRac :: Racional -> Racional -> Racional
resRac (x1,y1) (x2,y2) = simplificar(x1*y2 - x2*y1, y1*y2)

muestraRac :: Racional -> String
muestraRac (x, y)
        | y' == 1 = show x'
        | otherwise = show x' ++ "/" ++ show y'
        where (x',y') = simplificar(x,y)

-- EJERCICIO 2
data Racional' = Rac Integer Integer
    deriving(Eq)

simplificar' :: Racional' -> Racional'
simplificar' (Rac n d) = (Rac (((signum d)*n) `div` m)  ((abs d) `div`m))
	where m = gcd n d

multRac' :: Racional' -> Racional' -> Racional'
multRac' (Rac x1 y1) (Rac x2 y2) = simplificar'(Rac (x1*x2) (y1*y2))

divRac' :: Racional' -> Racional' -> Racional'
divRac' (Rac x1 y1) (Rac x2 y2) = simplificar'(Rac (x1*y2) (y1*x2))

sumRac' :: Racional' -> Racional' -> Racional'
sumRac' (Rac x1 y1) (Rac x2 y2) = simplificar'(Rac (x1*y2 + x2*y1) (y1*y2))

resRac' :: Racional' -> Racional' -> Racional'
resRac' (Rac x1 y1) (Rac x2 y2) = simplificar'(Rac (x1*y2 - x2*y1) (y1*y2))

instance Show Racional' where
    show (Rac x 1) = show x
    show (Rac x y) = show x' ++ "/" ++ show y'
        where (Rac x' y') = simplificar'(Rac x y)

-- EJERCICIO 3
instance Num Racional' where
    (*) = multRac'
    (+) = sumRac'
    (-) = resRac'
    negate (Rac x y) = simplificar'(Rac (-x) y)
    fromInteger x = (Rac x 1)
    signum (Rac n d) = if (n*d)<0 then -1 else 1  
    abs (Rac n d) = if (n*d)<0 then simplificar'(Rac (-n) d) else simplificar'(Rac n d)

-- NATURALES
data Nat = Cero | Succ Nat
    deriving(Eq,Show)

-- EJERCICIO 1
instance Num Nat where
    n + Cero = n
    n + Succ m = Succ (n+m)
    n - Cero = n
    Cero - n = Cero
    Succ n - Succ m = n-m
    n * Cero = Cero
    n * Succ m = Succ (n*m)
    abs n = n
    signum Cero = Cero
    signum (Succ n) = Succ Cero
    fromInteger x
        | x <= 0 = Cero
        | otherwise = Succ(fromInteger(x-1))

-- EJERCICIO 2
instance Ord Nat where
    Cero < Cero = False
    Cero < Succ n = True
    Succ n < Cero = False
    Succ n < Succ m = n < m

-- EJERCICIO 3
divModN :: Nat -> Nat -> (Nat,Nat)
divModN x y
        | y == Cero = undefined
        | x < y = (Cero, x)
        | otherwise = (Succ q, r)
                where (q,r) = divModN (x-y) y