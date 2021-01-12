-- I1M 2018-19: Rel_7.hs 
-- El algoritmo de Luhn
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- � Introducci�n                                                     --
-- ---------------------------------------------------------------------

-- El objetivo de esta relaci�n es estudiar un algoritmo para validar
-- algunos identificadores num�ricos como los n�meros de algunas tarjetas
-- de cr�dito; por ejemplo, las de tipo Visa o Master Card.  
--
-- El algoritmo que vamos a estudiar es el algoritmo de Luhn consistente
-- en aplicar los siguientes pasos a los d�gitos del n�mero de la
-- tarjeta.    
--    1. Se invierten los d�gitos del n�mero; por ejemplo, [9,4,5,5] se
--       transdforma en [5,5,4,9].
--    2. Se duplican los d�gitos que se encuentra en posiciones impares
--       (empezando a contar en 0); por ejemplo, [5,5,4,9] se transforma
--       en [5,10,4,18].
--    3. Se suman los d�gitos de cada n�mero; por ejemplo, [5,10,4,18]
--       se transforma en 5 + (1 + 0) + 4 + (1 + 8) = 19.
--    4. Si el �ltimo d�gito de la suma es 0, el n�mero es v�lido; y no
--       lo es, en caso contrario. 
--
-- A los n�meros v�lidos, los llamaremos n�meros de Luhn. 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la funci�n
--    digitosInv :: Integer -> [Integer]
-- tal que (digitosInv n) es la lista de los d�gitos del n�mero n. en
-- orden inverso. Por ejemplo, 
--    digitosInv 320274  ==  [4,7,2,0,2,3]
-- ---------------------------------------------------------------------

digitosInv :: Integer -> [Integer]
digitosInv n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n
--    doblePosImpar :: [Integer] -> [Integer]
-- tal que (doblePosImpar ns) es la lista obtenida doblando los
-- elementos en las posiciones impares (empezando a contar en cero y
-- dejando igual a los que est�n en posiciones pares. Por ejemplo,
--    doblePosImpar [4,9,5,5]    ==  [4,18,5,10] 
--    doblePosImpar [4,9,5,5,7]  ==  [4,18,5,10,7]
-- ---------------------------------------------------------------------

doblePosImpar :: [Integer] -> [Integer]
doblePosImpar = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la funci�n
--    sumaDigitos :: [Integer] -> Integer
-- tal que (sumaDigitos ns) es la suma de los d�gitos de ns. Por
-- ejemplo, 
--    sumaDigitos [10,5,18,4] = 1 + 0 + 5 + 1 + 8 + 4 =
--                            = 19
-- ---------------------------------------------------------------------

sumaDigitos :: [Integer] -> Integer
sumaDigitos ns = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n  
--    ultimoDigito :: Integer -> Integer
-- tal que (ultimoDigito n) es el �ltimo d�gito de n. Por ejemplo,
--    ultimoDigito 123 == 3
--    ultimoDigito   0 == 0
-- ---------------------------------------------------------------------

ultimoDigito :: Integer -> Integer
ultimoDigito n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la funci�n 
--    luhn :: Integer -> Bool
-- tal que (luhn n) se verifica si n es un n�mero de Luhn. Por ejemplo,
--    luhn 5594589764218858  ==  True
--    luhn 1234567898765432  ==  False
-- ---------------------------------------------------------------------

luhn :: Integer -> Bool
luhn n = undefined
