--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-} 

import Prelude hiding ((++),concat,reverse,length,null,elem,head,last,tail,init,take,drop,(!!),map,filter,sum,zip,unzip,and,foldr,foldl,foldr1,foldl1,scanl,scanr)

cuadrado :: Num a => a -> a
cuadrado x = x*x

-- FUNCIONES TEMA 6 --
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

head, last  :: [a] -> a
tail, init  :: [a] -> [a]
head (x:xs) = x
tail (x:xs) = xs
last (x:xs) = if null xs then x else last xs
init (x:xs) = if null xs then [] else x : init xs

-- EJERCICIO: Reescribir las funciones last e init como composición de head, tail y/o reverse:
last_ = head.reverse
init_ = reverse.tail.reverse


-----------------------------------------------------------------------------------------

take, drop  :: Int -> [a] -> [a]
take 0 xs = []
take (n+1) [] = []
take (n+1) (x:xs) = x : take n xs
drop 0 xs = xs
drop (n+1) [] = []
drop (n+1) (x:xs) = drop n xs

(!!)  :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! (n+1) = xs !! n

map  :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
filter  :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)=if p x then x:filter p xs else filter p xs

-- EJERCICIO: Definir una función sum que suma los elementos de una lista:
sum :: Num a => [a]->a
sum [] =0
sum (x:xs)=x+sum xs
--------------------------------------------------------------------------------- 
-- EJERCICIO: Definir una función listInt que cree una lista de enteros desde n hasta m:
listInt :: Integral a => a -> a -> [a]
listInt n m 
    |n>m=[]
    |otherwise = n:listInt (n+1) m
-------------------------------------------------------------------------------
-- EJERCICIO: Escribir en la terminal una expresión que calcule la suma de los cuadrados de los números enteros entre 1 y 100:
-- -- (sum.map cuadrado)(listInt 1 100)
-- EJERCICIO: Escribir en la terminal una expresión que calcule la suma de los cuadrados de los números pares entre 1 y 10:
-- -- (sum.map cuadrado.filter even)(listInt 1 10)
-------------------------------------------------------------------------------------------------

zip  :: [a] -> [b] -> [(a,b)]
zip [] ys = []
zip (x:xs) [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys
unzip  :: [(a,b)] -> ([a],[b])
unzip xs = (map fst xs, map snd xs)

-- EJERCICIO: Definir una función que calcule el producto escalar de dos vectores xs e ys:
pe :: (Num a)=> [a]->[a] ->a
pe xs ys = (sum.map mult) (zip xs ys) where mult (a,b)=a*b
------------------------------------------------------------------------------------
-- EJERCICIO: Definir la función and que haga el && de una lista de booleanos:
and::[Bool] -> Bool
and []=True
and (x:xs)=x && and xs
---------------------------------------------------------------------------------------
-- EJERCICIO: Definir una función nodec para determinar si una secuencia [x_0,...,x_{n-1}] es no decreciente:
nodec :: Ord a => [a] -> Bool
nodec xs = and(map me (zip xs (tail xs))) where me (a,b)= a<=b
----------------------------------------------------------------------------------------------

-- PLEGADO POR LA DERECHA:
foldr     :: (a -> b -> b) -> b -> [a] -> b
foldr f e [] = e
foldr f e (x:xs) = f x (foldr f e xs)

-- Redefinimos:
concat_ :: [[a]] -> [a]
concat_ = foldr (++) []

reverse_ :: [a] -> [a]
reverse_ = foldr snoc []
            where snoc x xs = xs ++ [x]

-- reverse_ [1,2,3]=foldr snoc [] [1,2,3]=snoc 1 (foldr snoc [] [2,3])=snoc 1 (snoc 2 (foldr snoc [] [3]))=
    -- snoc 1 (snoc 2 (snoc 3 (foldr snoc [] [])))=snoc 1 (snoc 2(snoc 3 []))=snoc 1(snoc 2 [3])=
        --snoc 1 [3,2] = [3,2,1]

length_ :: [a] -> Int
length_ = foldr increm 0
            where increm x n  = 1 + n

sum_ :: Num a => [a] -> a
sum_ = foldr (+) 0

and_ :: [Bool] -> Bool
and_ = foldr (&&) True

map_ :: (a -> b) -> [a] -> [b]
map_ f = foldr (cons.f) []
            where cons x xs = x:xs

unzip_ :: [(a,b)] -> ([a],[b])
unzip_ = foldr conss ([],[])
            where conss (x,y) (xs,ys) = (x:xs,y:ys)

-- PLEGADO POR LA IZQUIERDA:
foldl     :: (b -> a -> b) -> b -> [a] -> b
foldl f e [] = e
foldl f e (x:xs) = foldl f (f e x) xs

reverse_' :: [a] -> [a]
reverse_' = foldl cons []
             where cons xs x = x:xs

--reverse_' [1,2,3] = foldl cons [] [1,2,3]= foldl cons (cons [] 1) [2,3]= foldl cons [1] [2,3] =foldl cons (cons [1] 2) [3]=
   -- foldl cons[2,1][3]= foldl cons (cons [2,1] 3)[]=  foldl cons [3,2,1] []= [3,2,1]          
---------------------------------------------------------------------------------------
foldr1     :: (a -> a -> a) -> [a] -> a
foldr1 f (x:xs) = if null xs then x else f x (foldr1 f xs)

foldl1     :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

inits_ = foldr f [[]] where f x xxs = [] : map (x:) xxs

scanl     :: (b -> a -> b) -> b -> [a] -> [b]
scanl f e = map (foldl f e) . inits

scanl_     :: (b -> a -> b) -> b -> [a] -> [b]
scanl_ f e [] = [e]
scanl_ f e (x:xs) = e : scanl_ f (f e x) xs

-- EJERCICIO: Cálculo de la lista de sumas acumuladas de una lista de números:

-------------------------------------------------------------------------------

-- EJERCICIO: Cálculo de la lista de factoriales asociada a una lista de números: 

---------------------------------------------------------------------------------

tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs

scanr f e = map (foldr f e) . tails

scanr_ f e [] = [e]
scanr_ f e (x:xs) = f x (head ys):ys
         where ys = scanr f e xs

inits1 (x:xs) = map (x:) (inits xs)

-- tails1=
tails1 [x] = [[x]]
tails1 (x:xs) = (x:xs) : tails1 xs

scanl1 f = map (foldl1 f) . inits1

scanr1 f = map (foldr1 f) . tails1

-----------------------------------------------------------------
-- Funciones utiles sobre cadenas:
-- words, lines :: String -> [String]

-- LISTAS INTENSIONALES ----------------------------
l = [2,4,7]
l' = [2 * x | x <- len,even x,x>3]
l'' = [even x|x<-l]
l_' = concat(map f l) where f x = [2*x | even x]
l__' = concat(map f l) where f x = if even x then [2*x] else []

-- Pensar como expresar map, filter y concat usando listas intensionales:
map f xs = [f x | x <- xs]
filter p xs = [x | x <- xs, p x]
concat xxs = [x | xs <- xxs, x <- xs]

-- PATRONES REFUTABLES

-- Ejemplo 1
data Forma = Circulo Float | Rectangulo Float Float
listaFormas = [Circulo 2.1, Rectangulo 2.1 3.2, Circulo 4.7]
-- Queremos calcular la suma de todos los radios de listaFormas:
sumaRadios :: [Forma] -> Float
sumaRadios fs = sum [r | Circulo r <- fs]

-- Ejemplo 2: Uso de tipos implicitos
lL = [[],[1],[2,3],[4],[5,6]]
-- Queremos concatenar todas las listas que tengan un unico elemnto de una lista de listas:
unitarias :: [[a]] -> [a]
unitarias xxs = [x | [x] <- xss]

-- Queremos crear una lista a partir de una lista de listas, solo con las cabezas de las listas no vacias:
cabezas :: [[a]] -> [a]
cabezas xxs = [x | (x:xs)<-xxs]

-- EJEMPLOS CON LISTAS INTENSIONALES:
-- 1) Definir una funcion que para cada n devuelva [[1],[1,2],[1,2,3],...,[1..n]]
triangNum :: Int -> [[Int]]
triangNum n = [[1..x] | x <- [1..n]]
-- 2) Definir la funcion qSort que implemente el algoritmo Quicksort
qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort(x:xs) = qSort[y | y<-xs, y <= x] ++ [x] ++ qSort[y | y <- xs, y>x]


-- ÁRBOLES BINARIOS -------------------------------------------------------

data Arbol a = Hoja a | Rama (Arbol a) (Arbol a)

a1 = Rama(Rama(Hoja 12)(Rama(Hoja 23)(Hoja 13)))(Hoja 10)

a2 :: Arbol [Integer]
a2 = Rama (Hoja [1,2,3]) (Hoja [4,5,6])

a3 :: Arbol (Arbol Integer)
a3 = Rama (Hoja a1) (Hoja a1)

tam :: Arbol a -> Int
tam (Hoja x) = 1
tam (Rama xt yt) = tam xt + tam yt

aplanar :: Arbol a -> [a]
aplanar (Hoja x) = [x]
aplanar (Rama xt yt) = aplanar xt ++ aplanar yt

altura :: Arbol a -> Int
altura (Hoja x) = 0
altura (Rama xt yt) = 1 + (altura xt `max` altura yt)

mapArbol :: (a -> b) -> Arbol a -> Arbol b
mapArbol f (Hoja x) = Hoja (f x)
mapArbol f (Rama xt yt)=Rama(mapArbol f xt)(mapArbol f yt)

-- ÁRBOLES BINARIOS AUMENTADOS -------------------------------------------------------

data ArbolA a = Hoja_a a | Rama_a Int (ArbolA a) (ArbolA a)

-- ÁRBOLES BINARIOS ETIQUETADOS -------------------------------------------------------

data ArbolB a = Hoja_e | Rama_e (ArbolB a) a (ArbolB a)

-- ÁRBOLES BINARIOS MOTÍCULO -------------------------------------------------------

data ArbolM a = Hoja_m | Rama_m a (ArbolM a) (ArbolM a)

-- ROSADELFAS -------------------------------------------------------

data Rosa a = Nodo a [Rosa a]


