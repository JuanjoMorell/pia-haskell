-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 1º examen de evaluación continua (2 de noviembre de 2017)
-- ---------------------------------------------------------------------

-- Nombre: 
--
-- Apellidos: 
-- 
-- uvus:

-- ----------------------------------------------------------------------------

-- Nota: La puntuación de cada ejercicio es 2.5 puntos.

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.List

-- ----------------------------------------------------------------------------
-- Ejercicio 1.1. La suma de la serie geométrica de razón r cuyo primer
-- elemento es r es
--     1 + r + r² + r³ + ...

-- Definir la función 
--    sumaSG :: Double -> Double -> Double
-- tal que (sumaSG r n) es la suma de los n+1 primeros términos de dicha
-- serie. Por ejemplo,
--    sumaSG 0.5 100   == 2.0
--    sumaSG (1/3) 100 == 1.5
--    sumaSG (1/4) 100 == 1.3333333333333333
--
-- Indicación: usar la función (**) para la potencia.
-- ----------------------------------------------------------------------------

sumaSG :: Double -> Double -> Double
sumaSG r n = sum [(r**n) | x <- [0..(n+1)]]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. La serie converge cuando |r| < 1 y su límite es
-- 1/(1-r). Definir la función 
--    errorSG :: Double -> Double -> Double
-- tal que (errorSG r x) es el menor número de términos de la serie
-- de razón r necesarios para obtener su límite con un error menor que
-- x. Por ejemplo, 
--    errorSG 0.5 0.001   == 10.0
--    errorSG (1/3) 0.001 == 6.0
--    errorSG (1/4) 0.001 == 5.0

-- ---------------------------------------------------------------------

errorSG :: Double -> Double -> Double
errorSG r x = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 2.1. Definir por recursión la función 
--    todosDiferentesR :: Eq a => [a] -> Bool
-- tal que (todosDiferentesR xs) comprueba si todos los elementos de la
-- lista xs son diferentes. Por ejemplo,
--    todosDiferentesR [1..20]         == True
--    todosDiferentesR (7:[1..20])     == False
--    todosDiferentesR "Buenas"        == True
--    todosDiferentesR "Buenas tardes" == False
-- ----------------------------------------------------------------------------

todosDiferentesR :: Eq a => [a] -> Bool
todosDiferentesR = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 2.2. Definir por composición de funciones la función 
--    todosDiferentesC :: Eq a => [a] -> Bool
-- tal que (todosDiferentesC xs) comprueba si todos los elementos de la
-- lista xs son diferentes. Por ejemplo,
-- ----------------------------------------------------------------------------

todosDiferentesC :: Eq a => [a] -> Bool
todosDiferentesC = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que ambas definiciones coinciden.
-- ----------------------------------------------------------------------------

propTD :: [Int] -> Bool
propTD xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3. Una lista se denomina "cuadrada" si se puede obtener
-- concatenando dos copias de una misma lista. Por ejemplo, "abab",
-- "aa" son lista cuadradas, y "aaa", "abba" no lo son.
-- Definir la función
--    esCuadrada :: (Eq a, Ord a) => [a] -> Bool
-- tal que (esCuadrada xs) compruebe se xs es una lista cuadrada.
-- Por ejemplo,
--    esCuadrada "aa"   == True
--    esCuadrada "aaa"  == False
--    esCuadrada "abab" == True
--    esCuadrada "abba" == False
-- ---------------------------------------------------------------------

esCuadrada :: (Eq a, Ord a) => [a] -> Bool
esCuadrada xs = undefined

-- ----------------------------------------------------------------------------

-- Ejercicio 4. Un niño quiere subir saltando una escalera. Con cada
-- salto que da puede subir 1, 2 o 3 peldaños. 
-- Definir la función
--    numeroFormas :: Int -> Int
-- tal que (numeroFormas n) calcule de cuántas formas puede subir una
-- escalera de n peldaños. Por ejemplo,
--    numeroFormas 2  == 2
--    numeroFormas 3  == 4
--    numeroFormas 4  == 7
--    numeroFormas 10 == 274
-- ----------------------------------------------------------------------------

numeroFormas :: Int -> Int
numeroFormas = undefined

-- ----------------------------------------------------------------------------
