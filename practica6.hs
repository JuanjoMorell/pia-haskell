--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

data ArbolB a = Hoja
    | Rama (ArbolB a) a (ArbolB a)
    deriving Show

miArbol = Rama (Rama (Rama Hoja 1 Hoja) 2 (Rama Hoja 3 Hoja)) 4 (Rama (Rama Hoja 5 Hoja) 6 (Rama Hoja 7 Hoja))

tama :: Ord a => ArbolB a -> Int
tama Hoja = 0
tama (Rama a1 x a2) = tama a1 + 1 + tama a2 

aplanar :: Ord a => ArbolB a -> [a]
aplanar Hoja = []
aplanar (Rama a1 x a2) = aplanar a1 ++ [x] ++ aplanar a2

pertenece :: Ord a => a -> ArbolB a -> Bool
pertenece e ab = elem e (aplanar ab)

insertar :: Ord a => a -> ArbolB a -> ArbolB a
insertar e Hoja = (Rama Hoja e Hoja)
insertar e (Rama a1 x a2)
                | e <= x = Rama (insertar e a1) x a2
                | otherwise = Rama a1 x (insertar e a2)

borrar :: Ord a => a -> ArbolB a -> ArbolB a
borrar e Hoja = Hoja
borrar e (Rama a1 x a2) 
                | e < x = Rama (borrar e a1) x a2
                | e > x = Rama a1 x (borrar e a2)
                | vacio a2 = a1
                | vacio a1 = a2
                | otherwise = juntar a1 a2

vacio :: ArbolB a -> Bool
vacio Hoja = True
vacio _ = False

juntar :: Ord a => ArbolB a -> ArbolB a -> ArbolB a
juntar a1 a2 = Rama a1 minimoD (borrar minimoD a2)
	where minimoD = minArbol a2 

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

crearArbol :: Ord a => [a] -> ArbolB a
crearArbol [] = Hoja
crearArbol (x:xs) = insertar x (crearArbol xs)

crearArbol' :: (Ord a) => [a] -> ArbolB a
crearArbol' xs = foldr insertar Hoja xs

ordenarConArbol :: Ord a => [a] -> [a]
ordenarConArbol = aplanar . crearArbol

subconjunto :: (Eq a) => [a] -> [a] -> Bool
subconjunto xs ys = length xs == length [x | x <- xs, elem x ys]

igualesC :: (Eq a) => [a] -> [a] -> Bool
igualesC [] [] = True
igualesC [] ys = False
igualesC xs [] = False
igualesC (x:xs) (y:ys) = x == y && igualesC xs ys