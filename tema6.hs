--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- OPERACIONES SOBRE LISTA
-- length xs -> size elementos de xs
-- xs ++ ys -> Concatenar xs y ys
-- concat xss -> Concatena las listas xss
-- map f xs -> Aplica la funcion f a todos los elementos de xs

-----------------------------------------------------

import Prelude hiding ((++),concat,reverse,length,null,elem,head,last,tail,init,take,drop,(!!),map,filter,sum,zip,unzip,and,foldr,foldl,foldr1,foldl1,scanl,scanr)

-- EJERCICIOS TEORIA
cuadrado :: Num a => a -> a
cuadrado x = x*x

(++)  :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

concat  :: [[a]] -> [a]
concat [] = []
concat (xs:xxs) = xs ++ concat xxs

reverse  :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

length  :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

null  :: [a] -> Bool
null [] = True
null (x:xs) = False

elem :: (Eq a) => a -> [a] -> Bool
elem n [] = False
elem n (x:xs) 
    | n == x = True
    | otherwise = elem n xs

head, last :: [a] -> a
tail, init :: [a] -> [a]
head (x:xs) = x
tail (x:xs) = xs
last (x:xs) = if null xs then x else last xs
init (x:xs) = if null xs then [] else x : init xs

-- Reescribir las funciones last e init como composicion
-- de head tail y/o reverse
last' :: [a] -> a
init' :: [a] -> [a]
last' = head . reverse
init' = reverse . tail . reverse

-- take x xs = coge los primeros x elementos de la lista
-- drop x xs = coge todos los elementos, menos los x primeros
take, drop  :: Int -> [a] -> [a]
take 0 xs = []
take (n+1) [] = []
take (n+1) (x:xs) = x : take n xs
drop 0 xs = xs
drop (n+1) [] = []
drop (n+1) (x:xs) = drop n xs

-- !!: Acceso a una posicion concreta de una lista
(!!)  :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! (n+1) = xs !! n

-- MAP: Aplica la funcion f a cada elemento de la lista
map  :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

-- FILTER:  Aplica el filtro p a cada elemento de la lista
filter  :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)=if p x then x:filter p xs else filter p xs

-- EJERCICIO Definir sum, suma los elementos de una lista
sum :: (Num a) => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

-- EJERCICIO Definir listInt, lista de enteros desde n hasta m
listInt :: (Integral a) => a -> a -> [a]
listInt n m
    | n > m = []
    | otherwise = n:listInt (n+1) m

-- ZIP:  Coge dos lista y crea una de pares
zip  :: [a] -> [b] -> [(a,b)]
zip [] ys = []
zip (x:xs) [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- UNZIP: Lista de parejas y devuelve una pareja de listas
unzip  :: [(a,b)] -> ([a],[b])
unzip xs = (map fst xs, map snd xs)

-- EJERCICIO Definir funcion de producto escalar
pe :: (Num a) => [a] -> [a] -> a
pe xs ys = sum(map mult (zip xs ys))
    where mult (a,b) = a * b

-- EJERCICIO Definir and de lista de booleanos
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

-- EJERCICIO Definir nodec, para determinar si una
-- secuencia es no decreciente
nodec :: Ord a => [a] -> Bool
nodec xs = and(map me (zip xs (tail xs))) where me (a,b) = a <= b

-- PLEGADO POR LA DERECHA
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f e [] = e
foldr f e (x:xs) = f x (foldr f e xs)

concat' :: [[a]] -> [a]
concat' = foldr (++) []

reverse' :: [a] -> [a]
reverse' = foldr snoc []
            where snoc x xs = xs ++ [x]

-- PLEGADO POR LA IZQUIERDA:
foldl     :: (b -> a -> b) -> b -> [a] -> b
foldl f e [] = e
foldl f e (x:xs) = foldl f (f e x) xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

inits_ = foldr f [[]] where f x xxs = [] : map (x:) xxs 

scanl     :: (b -> a -> b) -> b -> [a] -> [b]
scanl f e = map (foldl f e) . inits

scanl_     :: (b -> a -> b) -> b -> [a] -> [b]
scanl_ f e [] = [e]
scanl_ f e (x:xs) = e : scanl_ f (f e x) xs

tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs

scanr f e = map (foldr f e) . tails

scanr_ f e [] = [e]
scanr_ f e (x:xs) = f x (head ys):ys
         where ys = scanr f e xs

-- funcion Words -> divide string por palabras
-- funcion lines -> divide string por lineas \n

-- Funcion quicksort con listas intensionales
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y<-xs,y<=x] ++ [x] ++ quicksort [y | y<-xs,y>x] 

-----------------------------------------------------

-- EJERCICIO 1
posiciones :: (Eq a) => a -> [a] -> [Integer]
posiciones x = map fst.(filter((==x).snd)).(zip [0..])

-- EJERCICIO 2
posicion :: (Eq a) => a -> [a] -> Integer
posicion x = head.(++[-1]).posiciones x

-- EJERCICIO 3
igualLista :: (Eq a) => [a] -> [a] -> Bool
igualLista [] [] = True
igualLista [] ys = False
igualLista xs [] = False
igualLista (x:xs) (y:ys) = x == y && igualLista xs ys

-- EJERCICIO 4
divisible :: Integer -> Integer -> Bool
divisible x y = x `rem` y == 0

divisores :: Integer -> [Integer]
divisores x = filter (divisible x) [1..x]

-- EJERCICIO 5
primo :: Integer -> Bool
primo x = length(divisores x) == 2

-- EJERCICIO 6
primos :: Integer -> [Integer]
primos x = filter primo [1..x]

-- EJERCICIO 7
data Dia = Domingo|Lunes|Martes|Miercoles|Jueves|Viernes|Sabado
	deriving (Show,Enum)


-- EJERCICIO 8
pitagoras :: (Int, Int, Int) -> Bool
pitagoras (x,y,z) = (x*x + y*y) == z*z

-- EJERCICIO 9
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- EJERCICIO 10

-- EJERCICIO 11