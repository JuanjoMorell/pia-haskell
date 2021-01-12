-- I1M 2018-19: Relación 14
-- El 2019 es feliz.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Según la Wikipedia (en http://bit.ly/2RBGEnz), un número feliz se
-- define por el siguiente proceso. Se comienza reemplazando el número
-- por la suma del cuadrado de sus dígitos y se repite el proceso hasta
-- que se obtiene el número 1 o se entra en un ciclo que no contiene al
-- 1. Aquellos números para los que el proceso termina en 1 se llaman
-- números felices y los que entran en un ciclo sin 1 se llaman números
-- desgraciados.  
-- 
-- Por ejemplo, 2019 es un número feliz porque
--    2019 ~> 2² + 0² + 1² + 9² =  4 +  1 + 81 = 86
--         ~> 8² + 6²           = 64 + 36      = 100
--         ~> 1² + 0² + 0²                     = 1 
-- Pero 17 es un número desgraciado porque
--    17 ~> 1² + 7²      =  1 + 49      =  50
--       ~> 5² + 0²      = 25 +  0      =  25
--       ~> 2² + 5²      =  4 + 25      =  29
--       ~> 2² + 9²      =  4 + 81      =  85
--       ~> 8² + 5²      = 64 + 25      =  89
--       ~> 8² + 9²      = 64 + 81      = 145
--       ~> 1² + 4² + 5² =  1 + 16 + 25 =  42
--       ~> 4² + 2²      = 16 +  4      =  20
--       ~> 2² + 0²      =  4 +  0      =   4
--       ~> 4²                          =  16
--       ~> 1² + 6²      =  1 + 36      =  37
--       ~> 3² + 7²      =  9 + 49      =  58
--       ~> 5² + 8²      = 25 + 64      =  89
-- que forma un bucle al repetirse el 89.
-- 
-- El objetivo del ejercicio es definir una función que calcule todos
-- los números felices hasta un límite dado y calcular la posición del
-- 2019 en dicha sucesión.

-- ---------------------------------------------------------------------
-- § Librerias auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Char
import Test.QuickCheck
import Data.Numbers.Primes

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    digitos :: Int -> [Int]
-- tal que (digitos n) es la lista de los dígitos de n. Por ejemplo, 
--    digitos 325  ==  [3,2,5]
-- ---------------------------------------------------------------------

digitos :: Int -> [Int]
digitos n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    caminoALaFelicidad :: Int -> [Int]
-- tal que (caminoALaFelicidad n) es la lista de los números obtenidos
-- en el proceso de la determinación si n es un número feliz: se
-- comienza con la lista [n], ampliando la lista con la suma del
-- cuadrado de los dígitos de su primer elemento y se repite el proceso
-- hasta que se obtiene el número 1 o se entra en un ciclo que no
-- contiene al 1. Por ejemplo,
--    λ> caminoALaFelicidad 2019
--    [1,100,86,2019]
--    λ> caminoALaFelicidad 17
--    [89,58,37,16,4,20,42,145,89,85,29,25,50,17]
-- ---------------------------------------------------------------------

caminoALaFelicidad :: Int -> [Int]
caminoALaFelicidad n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    esFeliz :: Int -> Bool
-- tal que (esFeliz n) se verifica si n es un número feliz. Por ejemplo,
--    esFeliz 2019  ==  True
--    esFeliz 17    ==  False
-- ---------------------------------------------------------------------

esFeliz :: Int -> Bool
esFeliz n = undefined

-- --------------------------------------------------------------------
-- Ejercicio 4. Definir la lista
--    numerosFelices :: [Int]
-- cuyos elementos son los números felices. Por ejemplo,
--    take 10 numerosFelices  ==  [1,7,10,13,19,23,28,31,32,44]
-- ---------------------------------------------------------------------

numerosFelices :: [Int]
numerosFelices = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    posicionFeliz :: Int -> Maybe Int
-- tal que (posicionFeliz n) es justo la posición de n en la sucesión de
-- los números felices, si n es un número feliz y Nothing, en caso
-- contrario. Por ejemplo,
--    posicionFeliz 2019    ==  Just 300
--    posicionFeliz 17      ==  Nothing
--    posicionFeliz (10^5)  ==  Just 14376
-- ---------------------------------------------------------------------

posicionFeliz :: Int -> Maybe Int
posicionFeliz n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6. Comprobar con QuickChek que existen infinitos números
-- primos felices; es decir, que para cada número natural n existen
-- números primos mayores que n.
--
-- Nota: La demostración de la infinitud de los primos felices es un
-- problema abierto.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_primosFelices :: Int -> Property
prop_primosFelices n = undefined

-- La comprobación es
