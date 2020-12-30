--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 1
bisiesto :: Integer -> Bool
bisiesto x
        | (x `mod` 100) == 0 && (x `mod` 400) == 0 = True
        | (x `mod` 4) == 0 = True
        | otherwise = False

-- EJERCICIO 2
data Triangulo = Escaleno | Isosceles | Equilatero | Ninguno
    deriving (Enum, Show)

triangulo :: Integer -> Integer -> Integer -> Triangulo
triangulo x y z
        | z > (x+y) = Ninguno
        | x == y && x == z = Equilatero
        | x == y = Isosceles
        | otherwise = Escaleno

-- EJERCICIO 3
isDigit, isLower, isUpper :: Char -> Bool
isDigit c = '0' <= c && c <= '9'
isLower c = 'a' <= c && c <= 'z'
isUpper c = 'A' <= c && c <= 'Z'

-- EJERCICIO 4 y 5
toMinus, toMayus :: Char -> Char
toMinus c = if isUpper c then toEnum(fromEnum c - fromEnum 'A' + fromEnum 'a') else c
toMayus c = if isLower c then toEnum(fromEnum c - fromEnum 'a' + fromEnum 'A') else c

-- EJERCICIO 6
par :: (a -> b, a -> c) -> a -> (b,c)
par (f,g) x = (f x,g x)

prod :: (a -> b, c -> d) -> (a,c) -> (b,d)
prod (f,g) = par(f.fst,g.snd)

replica :: Char -> (Char,Char)
replica c
    | isLower c || isUpper c = par (toMinus, toMayus) c
    | otherwise = (c,c)

primMinsegMay :: (Char,Char) -> (Char,Char)
primMinsegMay = prod (toMinus, toMayus)

-- EJERCICIO 7
raicesReales :: (Float, Float, Float) -> (Float, Float)
raicesReales (a,b,c)
            | a == 0 = error "No es raiz de segundo grado"
            | e < 0 = error "Raiz compleja"
            | otherwise = ((-b + e) / 2*a, (-b - e) / 2*a)
                where e = b^2 - 4*a*c

-- EJERCICIO 8
type Coeficientes = (Float, Float, Float)
type Raices = (Float, Float)

raices :: Coeficientes -> Raices
raices = raicesReales

-- EJERCICIO 9
-- x -> y -> x
-- (a -> b -> c) -> (a -> b) -> a -> c
-- (a -> b) -> a -> b
-- (a -> b -> c) -> b -> a -> c

-- EJERCICIO 10
-- swap :: (a,b) -> (b,a)

-- EJERCICIO 11


-- EJERCICIO 12
(~=) :: Float -> Float -> Bool
(~=) x y = abs (x-y) < 0.0001

-- EJERCICIO 13
until2 :: (a -> Bool) -> (a -> a) -> a -> a
until2 p f x = if p x then x else until2 p f (f x) 

-- EJERCICIO 14
data Direccion = Sur | Norte | Este | Oeste | Ninguna
    deriving(Enum,Eq,Show)

mueve :: Direccion -> (Int,Int) -> (Int,Int)
mueve dir (x,y)
        | dir == Sur = (x-1, y)
        | dir == Norte = (x+1, y)
        | dir == Este = (x, y-1)
        | dir == Oeste = (x, y+1)
        | otherwise = (x,y)

-- EJERCICIO 15
movimiento :: (Int,Int) -> (Int,Int) -> (Direccion,Direccion)
movimiento (x,y) (x',y')
        | x < x' && y < y' = (Norte,Oeste)
        | x < x' && y > y' = (Norte,Este)
        | x < x' && y == y' = (Norte, Ninguna)
        | x > x' && y < y' = (Sur,Oeste)
        | x > x' && y > y' = (Sur,Este)
        | x > x' && y == y' = (Sur, Ninguna)
        | x == x' && y < y' = (Ninguna,Oeste)
        | x == x' && y > y' = (Ninguna,Este)
        | otherwise = (Ninguna, Ninguna)

-- EJERCICIO 17
data Dia = Lun | Mar | Mier | Juev | Vier | Sab | Dom
		deriving (Enum,Show)

instance Eq Dia where
    a == b = fromEnum a == fromEnum b

instance Ord Dia where
    a <= b = fromEnum a <= fromEnum b

laborable, festivo :: Dia -> Bool
laborable d = Lun <= d && d <= Vier
festivo d = d == Sab || d == Dom

diaDespues :: Dia -> Dia
diaDespues d = toEnum ((fromEnum d + 1) `mod` 7)

-- EJERCICIO 18
data Forma = Circulo Float
    | Rectangulo Float Float
    deriving (Eq)

area :: Forma -> Float
area (Circulo r) = r*r*pi
area (Rectangulo x y) = x * y

-- EJERCICIO 19
data Circulo1 = Circ Float
data Rectangulo1 = Rect Float Float

type Forma1 = Either Circulo1 Rectangulo1

areaCirculo :: Circulo1 -> Float
areaCirculo (Circ x) = pi * x * x

areaRectangulo :: Rectangulo1 -> Float
areaRectangulo (Rect x y) =x * y

area2 :: Forma1 -> Float
area2 = either areaCirculo areaRectangulo
