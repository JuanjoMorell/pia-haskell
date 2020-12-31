--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- RACIONALES

-- EJERCICIO 1
type Racional = (Integer,Integer)

simplificarRac :: Racional -> Racional
simplificarRac  (n,d) = (((signum d)*n) `div` m, (abs d) `div`m)
    where m = gcd n d

multRac :: Racional -> Racional -> Racional
multRac (n,d) (n',d') = simplificarRac (n*n',d*d')

divRac :: Racional -> Racional -> Racional
divRac (n,d) (n',d') = simplificarRac (n*d',d*n')

sumRac :: Racional -> Racional -> Racional
sumRac (n,d) (n',d') = simplificarRac (n*d'+ n'*d,d*d')

resRac :: Racional -> Racional -> Racional
resRac (n,d) (n',d') = simplificarRac (n*d'- n'*d,d*d')

muestraRac :: Racional -> String
muestraRac (n,d)
    | d == 1 = show n
    | n == d = show 1
    | otherwise = show n' ++ "/" ++ show d'
    where (n',d') = simplificarRac (n,d)

-- EJERCICIO 2
data Racional2 = Rac Integer Integer

simplificar2 :: Racional2 -> Racional2
simplificar2 (Rac n d) = (Rac (((signum d)*n) `div` m)  ((abs d) `div`m))
	where m = gcd n d

r2Mul :: Racional2 -> Racional2 -> Racional2
r2Mul (Rac n1 d1) (Rac n2 d2) = simplificar2 (Rac (n1*n2) (d1*d2))

r2Div :: Racional2 -> Racional2 -> Racional2
r2Div (Rac n1 d1) (Rac n2 d2) = simplificar2 (Rac (n1*d2) (d1*n2)) 

r2Sum :: Racional2 -> Racional2 -> Racional2
r2Sum (Rac n1 d1) (Rac n2 d2) = simplificar2 (Rac (n1*d2+n2*d1) (d1*d2)) 

r2Res :: Racional2 -> Racional2 -> Racional2
r2Res (Rac n1 d1) (Rac n2 d2) = simplificar2 (Rac (n1*d2-n2*d1) (d1*d2)) 

instance Show Racional2 where
    show (Rac n 1) = show n
    show (Rac n d) = show n' ++ "/" ++ show d'
        where (Rac n' d') = simplificar2 (Rac n d)

-- EJERCICIO 3
instance Num Racional2 where
    (+) = r2Sum
    (-) = r2Res
    (*) = r2Mul
    negate (Rac n d) = simplificar2 (Rac (-n) d) 
    fromInteger n = (Rac n 1)
    signum (Rac n d) = if (n*d) < 0 then -1 else 1
    abs (Rac n d) = if (n*d) < 0 then simplificar2 (Rac (-n) d) else simplificar2 (Rac n d)

    -- NATURALES
    data Nat = Cero | Succ Nat
        deriving (Eq, Show)

    instance Num Nat where
        n + Cero =  n
        n + Succ m = Succ (n+m)
        n - Cero = n
        Cero - Succ n = Cero
        Succ n - Succ m = n - m
        n * Cero = Cero
        n * Succ m = n * m + n
        