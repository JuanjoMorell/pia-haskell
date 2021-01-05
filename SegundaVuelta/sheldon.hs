--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 1
divisible :: Int -> [Int]
divisible n = [x | x <- [1..n], n `mod` x == 0]

primo :: Int -> Bool
primo n = (length (divisible n)) == 2

-- EJERCICIO 2
primos :: [Int]
primos = [x | x <- [1,2..], primo x]

-- EJERCICIO 3
dividirNum :: Int -> [Int]
dividirNum 0 = []
dividirNum x = dividirNum (x `div` 10) ++ [x `mod` 10]

productoDigitos :: Int -> Int
productoDigitos n = product (dividirNum n)

-- EJERCICIO 4
inverso :: Int -> Int
inverso = read . reverse . show

-- EJERCICIO 5
esPrimoSheldon :: Int -> Bool
esPrimoSheldon x = n > 0
  && x == primos !! (n - 1)
  && inverso x == primos !! (inverso n - 1)
  where n = productoDigitos x

esPrimoSheldon' :: Int -> Bool
esPrimoSheldon' n = primo n && n == (productoDigitos n)

-- EJERCICIO 6
primoSheldon :: Int
primoSheldon = head [x | x <- [1,2..], esPrimoSheldon x]

primoSheldon' :: Int
primoSheldon' = head [x | x <- [1,2..], esPrimoSheldon' x]