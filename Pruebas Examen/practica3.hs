--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 1
type Racional = (Int,Int)

mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

simplificar :: Racional -> Racional
simplificar (n,d) = (n `div` divisor, d `div` divisor)
    where divisor = mcd n d

multRac :: Racional -> Racional -> Racional
multRac (n1,d1) (n2,d2) = simplificar (n1*n2, d1*d2)

divRac :: Racional -> Racional -> Racional
divRac (n1,d1) (n2,d2) = simplificar (n1*d2, n2*d1)

sumRac :: Racional -> Racional -> Racional
sumRac (n1,d1) (n2,d2) = simplificar (n1*d2 + n2*d1, d1*d2)

resRac :: Racional -> Racional -> Racional
resRac (n1,d1) (n2,d2) = simplificar (n1*d2 - n2*d1, d1*d2)

muestraRac :: Racional -> String
muestraRac (n,d) = if d' == 1 then show n' else show n' ++ "/" ++ show d'
    where (n',d') = simplificar(n,d)

-- EJERCICIO 2
data Racional' = Rac Integer Integer

mcd' :: Integer -> Integer -> Integer
mcd' a 0 = a
mcd' a b = mcd' b (a `mod` b)

simplificar' :: Racional' -> Racional'
simplificar' (Rac n d) = (Rac (n `div` divisor) (d `div` divisor))
    where divisor = mcd' n d

multRac' :: Racional' -> Racional' -> Racional'
multRac' (Rac n1 d1) (Rac n2 d2) = simplificar' (Rac (n1*n2) (d1*d2))

divRac' :: Racional' -> Racional' -> Racional'
divRac' (Rac n1 d1) (Rac n2 d2) = simplificar' (Rac (n1*d2) (n2*d1))

sumRac' :: Racional' -> Racional' -> Racional'
sumRac' (Rac n1 d1) (Rac n2 d2) = simplificar' (Rac (n1*d2+n2*d1) (d1*d2))

resRac' :: Racional' -> Racional' -> Racional'
resRac' (Rac n1 d1) (Rac n2 d2) = simplificar' (Rac (n1*d2-n2*d1) (d1*d2))

instance Show Racional' where
    show (Rac n 1) = show n
    show (Rac n d) = show n ++ "/" ++ show d

-- EJERCICIO 3
instance Num Racional' where
    (+) = sumRac'
    (-) = resRac'
    (*) = multRac'
    negate (Rac n d) = (Rac (-n) d)
    fromInteger n = (Rac n 1)
    abs (Rac n d) = if (n*d) < 0 then simplificar'(Rac (-n) d) else simplificar'(Rac n d)
    signum (Rac n d) = if (n*d) < 0 then -1 else 1

-- NATURALES
-- EJERCICIO 1
data Nat = Cero | Succ Nat
    deriving (Eq)

-- EJERCICIO 1
instance Num Nat where
    n + Cero = n
    n + Succ m = Succ (n + m)
    n - Cero = n
    Cero - Succ m = Cero
    Succ n - Succ m = n - m
    n * Cero = Cero
    n * Succ m = (n * m) + n
    abs n = n
    fromInteger x
		| x <= 0 = Cero
		| otherwise = Succ (fromInteger (x-1))
    
instance Ord Nat where
	Cero < Cero = False
	Cero < Succ n = True
	Succ n < Cero = False
	Succ n < Succ m = n < m

-- EJERCICIO 3
divModN :: Nat -> Nat -> (Nat,Nat)
divModN x y
	| y == Cero = undefined
	| x < y = (Cero,x)
 	| otherwise = (Succ q, r)
		where (q,r) = divModN (x-y) y

-- EJERCICIO 4
instance Show Nat where 
	show Cero = "0"
	show (Succ n) = show(read (show n)+1)