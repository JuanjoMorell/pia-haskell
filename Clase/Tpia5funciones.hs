--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-} 

-- FUNCIONES TEMA 5 --

cuadrado :: Int -> Int
cuadrado x = x * x

mayusc :: Char -> Char
mayusc c = toEnum(fromEnum c-fromEnum 'a'+fromEnum 'A')

type Nombre = String
type Edad = Integer
--String ya está definido en Prelude:
--type String = [Char]
type Persona = (Nombre,Edad)

tocayos :: Persona -> Persona -> Bool
tocayos (nombre,_) (nombre',_)=nombre==nombre'

data Persona2 = Pers Nombre Edad

juan::Persona2
juan = Pers "Juan Lopez" 23


esJoven:: Persona2 -> Bool
esJoven (Pers _ edad) = edad < 25

verPersona::Persona2 -> String
verPersona (Pers nombre edad) = "Nombre: " ++ nombre ++ ", edad: " ++ show edad

data Persona3 = Datos {nombre::Nombre, dni::Integer, edad::Edad}

tocayos3:: Persona3 -> Persona3 -> Bool
tocayos3 p p' = nombre p == nombre p'

data Color = Rojo | Verde | Azul

data Temperatura = Frio | Caliente

data Estacion = Primavera | Verano | Otonio | Invierno

tiempo :: Estacion -> Temperatura
tiempo Primavera = Caliente

tiempo Verano = Caliente
tiempo _ = Frio

data Forma = Circulo Float
	| Rectangulo Float Float

area :: Forma -> Float
area (Circulo radio) = pi * radio * radio
area (Rectangulo base altura) = base * altura

data Expr = Lit Integer
 | Suma Expr Expr
 | Resta Expr Expr
eval (Lit n) = n
eval (Suma e1 e2) = eval e1 + eval e2
eval (Resta e1 e2) = eval e1 * eval e2

newtype Persona4 = Per (Nombre, Edad)

esJoven' (Per (_, edad)) = edad < 25

---------------------------------------------------------

data Dia = Lun | Mar | Mier | Juev | Vier | Sab | Dom
	--deriving (Enum,Show)
 
instance Enum Dia where
 fromEnum Lun = 0
 fromEnum Mar = 1
 fromEnum Mier = 2
 fromEnum Juev = 3
 fromEnum Vier = 4
 fromEnum Sab = 5
 fromEnum Dom = 6 
 toEnum 0 = Lun 
 toEnum 1 = Mar
 toEnum 2 = Mier 
 toEnum 3 = Juev 
 toEnum 4 = Vier 
 toEnum 5 = Sab 
 toEnum  6 = Dom

data Dato = Par Bool Int
--	deriving (Show)

showDato :: Dato -> String
showDato (Par b n )
     = if b then "+" ++ show n ++ "\n" else "-" ++ show n ++ "\n"
putDato = putStr . showDato
 









