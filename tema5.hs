--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 1
bisiesto :: Integer -> Bool
bisiesto x = (x `mod` 100 == 0 && x `mod` 400 == 0) || (x `mod` 4 == 0)

-- EJERCICIO 2
data Triangulo = Fallo | Isosceles | Equilatero | Escaleno
    deriving(Show)

triangulo :: (Integer, Integer, Integer) -> Triangulo
triangulo (x,y,z)
    | x + y <= z = Fallo
    | x == y && x == z = Equilatero
    | x == y || y == z || x == z = Isosceles
    | otherwise = Escaleno

-- EJERCICIO 3
isDigit, isLower, isUpper :: Char -> Bool
isDigit c = '0' <= c && c <= '9'
isLower c = 'a' <= c && c <= 'z'
isUpper c = 'A' <= c && c <= 'z'

-- EJERCICIO 4
-- fromEnum: Convierte un caracter en número.
-- toEnum: Convierte un numero en caracter.
minusc :: Char -> Char
minusc c = toEnum(fromEnum c - fromEnum 'A' + fromEnum 'a' )

mayusc :: Char -> Char
mayusc c = toEnum(fromEnum c + fromEnum 'A' - fromEnum 'a' )

-- EJERCICIO 5
f, g :: Char -> Char
f c = if isUpper c then minusc c else c
g c = if isLower c then mayusc c else c

-- EJERCICIO 6
replica :: Char -> (Char, Char)
replica c
    | isLower c = (c, mayusc c)
    | isUpper c = (minusc c , c)
    | otherwise = (c,c)

isChar :: Char -> Bool
isChar c = c <= 'a' && c <= 'Z'

primMinsegMay :: (Char, Char) -> (Char, Char)
primMinsegMay (c1, c2)
            | isChar c1 && isChar c2 = (minusc c1, mayusc c2)
            | otherwise = (c1, c2)

-- EJERCICIO 7
segundoGrado :: (Float, Float, Float) -> (Float, Float)
segundoGrado (a,b,c)
        | a == 0 = error "No es de segundo grado"
        | e < 0 = error "Raices complejas"
        | otherwise = ((-b-r)/d, (-b+r)/d)
        where r = sqrt e
              d = 2 * a
              e = b*b-4*a*c

-- EJERCICIO 8


-- EJERCICIO 9
--const :: a -> b -> a
--subst :: (a -> b -> c) -> (a -> b) -> a -> c
--aplicar :: (a -> b) -> a -> b
--flip :: (a -> b -> c) -> b -> a -> c

-- EJERCICIO 10
-- flip(curry f) x y = curry(f. swap) x y
-- (curry f) y x       (f. swap)(x,y)
-- f (y,x)              f ( swap (x,y))
swap :: (Integer,Integer) -> (Integer,Integer)
swap (x,y) = (y,x)

-- EJERCICIO 11
-- g :: a -> b
-- rara :: ((a -> b) -> a) -> (a -> b) -> b
-- masrara no se puede definir

-- EJERCICIO 12
(~=) :: Float -> Float -> Bool
(~=) x y = absoluto <= 0.0001
    where absoluto = abs (x - y)

-- EJERCICIO 13
until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f x = if p x then x else until' p f (f x)

-- EJERCICIO 14
data Direccion = Sur | Norte | Este | Oeste | Ninguna
    deriving(Enum, Show)

mueve :: Direccion -> (Integer, Integer) -> (Integer, Integer)
mueve Sur (x,y) = (x,y-1)
mueve Norte (x,y) = (x,y+1)
mueve Este (x,y) = (x+1,y)
mueve Oeste (x,y) =(x-1,y)

-- EJERCICIO 15
movimiento :: (Integer, Integer) -> (Integer, Integer) -> (Direccion, Direccion)
movimiento (x1,y1) (x2,y2) = (if x1==x2 then Ninguna else if x1 < x2 then Norte else Sur, if y1==y2 then Ninguna else if y1 < y2 then Este else Oeste)

-- EJERCICIO 16
cuadrado :: Num a => a -> a
cuadrado x = x * x

-- EJERCICIO 17
data Dia = Lun | Mar | Mier | Juev | Vier | Sab | Dom
	deriving (Enum,Show)

instance Eq Dia where
    d1 == d2 = fromEnum d1 == fromEnum d2

instance Ord Dia where
    d1 <= d2 = fromEnum d1 <= fromEnum d2

laborable, festivo :: Dia -> Bool
laborable d = d >= Lun && d <= Vier
festivo d = d == Sab || d == Dom

laborable', festivo' :: Dia -> Bool
laborable' d = d <= Vier
festivo' d = not (laborable' d)

diaDespues :: Dia -> Dia
diaDespues d = toEnum(mod (fromEnum d + 1) 7)

-- EJERCICIO 18
data Forma = Circulo Float
    | Rectangulo Float Float

area :: Forma -> Float
area (Circulo radio) = pi * radio * radio
area (Rectangulo base altura) = base * altura

instance Eq Forma where
    f1 == f2 = area f1 == area f2
    f1 /= f2 = not(f1 == f2)
    

-- EJERCICIO 19
data Circulo' = Cir Float
data Rectangulo' = Rec Float Float

type Forma1 = Either Circulo' Rectangulo'

area' :: Forma1 -> Float
area' (Right (Rec alto ancho)) = ancho * alto
area' (Left (Cir radio)) = radio * radio * pi

areaCirculo :: Circulo' -> Float
areaCirculo (Cir radio) = radio * radio * pi

areaRectangulo :: Rectangulo' -> Float
areaRectangulo (Rec alto ancho) = alto * ancho

area'' :: Forma1 -> Float
area'' = either areaCirculo areaRectangulo


