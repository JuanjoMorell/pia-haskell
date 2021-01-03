--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Practica6 where
	
data ArbolB a = Hoja
    | Rama (ArbolB a) a (ArbolB a)
    deriving Show

miArbol = Rama (Rama (Rama Hoja 1 Hoja) 2 (Rama Hoja 3 Hoja)) 4 (Rama (Rama Hoja 5 Hoja) 6 (Rama Hoja 7 Hoja))

tama :: ArbolB a -> Int
tama Hoja = 0
tama (Rama a1 x a2) = (tama a1) + 1 + (tama a2)

aplanar :: ArbolB a -> [a]
aplanar Hoja = []
aplanar (Rama a1 x a2) = aplanar a1 ++ [x] ++ aplanar a2

pertenece :: (Eq a) => a -> ArbolB a -> Bool
pertenece a Hoja = False
pertenece a (Rama a1 x a2) = if a == x then True else (pertenece a a1) || (pertenece a a2)

insertar :: (Ord a) => a -> ArbolB a -> ArbolB a
insertar a Hoja = (Rama Hoja a Hoja)
insertar a (Rama a1 x a2)
        | a <= x = Rama (insertar a a1) x a2
        | a > x = Rama a1 x (insertar a a2)

borrar :: (Ord a) => a -> ArbolB a -> ArbolB a
borrar e Hoja = Hoja
borrar e (Rama iz x dr)
	| e < x = Rama  (borrar e iz) x dr
	| e > x = Rama iz x (borrar e dr)
	| vacio dr = iz
	| vacio iz = dr
	| otherwise = juntar iz dr 

vacio :: ArbolB a -> Bool
vacio Hoja = True
vacio _ = False

juntar :: Ord a => ArbolB a -> ArbolB a -> ArbolB a
juntar iz dr = Rama iz minimoD (borrar minimoD dr)
	where minimoD = minArbol dr 


minArbol :: Ord a => ArbolB a ->  a
minArbol t
	| vacio (hijoIzq t) = valorRaiz t
	| otherwise = minArbol (hijoIzq t)

hijoIzq ::  Ord a => ArbolB a -> ArbolB a
hijoIzq Hoja = error "No tiene hijo izquierdo\n"
hijoIzq (Rama t1 _ _) = t1

valorRaiz :: Ord a => ArbolB a -> a
valorRaiz Hoja = error "No hay valor en la raiz porque el arbol es vacio\n"
valorRaiz (Rama _ x _) = x

crearArbolL :: (Ord a) => [a] -> ArbolB a
crearArbolL [] = Hoja
crearArbolL (x:xs) = insertar x (crearArbolL xs)

ordenarConArbol :: (Ord a) => [a] -> [a]
ordenarConArbol = aplanar . crearArbolL

subconjunto :: (Ord a) => [a] -> [a] -> Bool
subconjunto xs [] = False
subconjunto [] ys = True
subconjunto (x:xs) ys = if elem x ys then True && (subconjunto xs ys) else False

igualesC :: (Ord a) => [a] -> [a] -> Bool
igualesC xs ys = subconjunto xs ys && subconjunto ys xs