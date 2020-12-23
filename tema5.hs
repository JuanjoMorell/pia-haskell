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
const :: a -> b -> a
subst :: (a -> b -> c) -> (a -> b) -> a -> c
aplicar :: (a -> b) -> a -> b
flip :: (a -> b -> c) -> b -> a -> c




