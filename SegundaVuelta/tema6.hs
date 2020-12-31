--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 1
posiciones :: (Eq a) => a -> [a] -> [Int]
posiciones c xs = [n | (x,n) <- (zip xs [1,2..]), x == c]

-- EJERCICIO 2
posicion :: (Eq a) => a -> [a] -> Int
posicion c xs = head (posiciones c xs)

-- EJERCICIO 3
igualLista :: Eq a => [a] -> [a] -> Bool
igualLista [] [] = True
igualLista xs [] = False
igualLista [] ys = False
igualLista (x:xs) (y:ys) = x == y && igualLista xs ys

-- EJERCICIO 4
divisores :: Integer -> [Integer]
divisores x = [n | n <- [1..x], x `mod` n == 0]

-- EJERCICIO 5
primo :: Integer -> Bool
primo x = length (divisores x) == 2

-- EJERCICIO 6
primos :: Integer -> [Integer]
primos n = [x | x <- [1..n], primo x]

-- EJERCICIO 7
data Dia = Domingo|Lunes|Martes|Miercoles|Jueves|Viernes|Sabado
	deriving (Show,Enum)

dia :: Int -> Int -> Int -> Dia
dia d m a = toEnum (numeroDeDias d m a `mod` 7)

numeroDeDias d m a = (a-1)*365
	+ numeroDeBisiestos a
	+ sum (take (m-1) (meses a))
	+ d

numeroDeBisiestos a = length (filter bisiesto [1..a-1])

bisiesto a = divisible a 4 && (not(divisible a 100) || divisible a 400)

meses a = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	where feb | bisiesto a = 29
		     | otherwise = 28