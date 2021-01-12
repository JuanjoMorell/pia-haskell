-- I1M 2018-19: Rel_3.hs 
-- Definiciones por comprension
-- Departamento de Ciencias de la Computacion e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introduccion                                                       --
-- ---------------------------------------------------------------------

-- En esta relacion se presentan ejercicios con definiciones por
-- comprension correspondientes al tema 5 que se encuentra
--    http://www.cs.us.es/~jalonso/cursos/i1m-18/temas/tema-5.html

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por comprension, la funcion 
--    sumaDeCuadrados :: Integer -> Integer 
-- tal que (sumaDeCuadrados n) es la suma de los cuadrados de los
-- primeros n numeros; es decir, 1^2 + 2^2 + ... + n^2. Por ejemplo,
--    sumaDeCuadrados 3    ==  14
--    sumaDeCuadrados 100  ==  338350
-- ---------------------------------------------------------------------

sumaDeCuadrados :: Integer -> Integer 
sumaDeCuadrados n = sum [x^2 | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir por comprension la funcion 
--    replica :: Int -> a -> [a]
-- tal que (replica n x) es la lista formada por n copias del elemento
-- x. Por ejemplo,  
--    replica 4 7     ==  [7,7,7,7]
--    replica 3 True  ==  [True, True, True]
-- Nota: La funcion replica es equivalente a la predefinida replicate.
-- ---------------------------------------------------------------------

replica :: Int -> a -> [a]
replica n x = [x | c <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la funcion 
--    suma :: Integer -> Integer
-- tal (suma n) es la suma de los n primeros numeros. Por ejemplo,
--    suma 3  ==  6
-- ---------------------------------------------------------------------

suma :: Integer -> Integer
suma n = sum [x | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Los triangulos aritmeticos se forman como sigue
--     1
--     2  3
--     4  5  6
--     7  8  9 10
--    11 12 13 14 15
--    16 17 18 19 20 21
-- Definir la funcion
--    linea :: Integer -> [Integer]
-- tal que (linea n) es la linea n-esima de los triangulos
-- aritmeticos. Por ejemplo,  
--    linea 4  ==  [7,8,9,10]
--    linea 5  ==  [11,12,13,14,15]
-- ---------------------------------------------------------------------

linea :: Integer -> [Integer]
linea n = [x | x <- [(s-(n-1))..s]]
    where s = suma n

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la funcion 
--    triangulo :: Integer -> [[Integer]]
-- tal que (triangulo n) es el triangulo aritmetico de altura n. Por
-- ejemplo, 
--    triangulo 3  ==  [[1],[2,3],[4,5,6]]
--    triangulo 4  ==  [[1],[2,3],[4,5,6],[7,8,9,10]]
-- ---------------------------------------------------------------------

triangulo :: Integer -> [[Integer]]
triangulo n = [linea x | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un entero positivo es perfecto si es igual a la suma de
-- sus factores, excluyendo el propio numero. 
-- 
-- Definir por comprension la funcion 
--    perfectos :: Int -> [Int]
-- tal que (perfectos n) es la lista de todos los numeros perfectos
-- menores que n. Por ejemplo,  
--    perfectos 500  ==  [6,28,496]
-- Indicacion: Usar la funcion factores del tema 5.
-- ---------------------------------------------------------------------

perfectos :: Int -> [Int]
perfectos n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Un numero natural n se denomina abundante si es menor
-- que la suma de sus divisores propios. Por ejemplo, 12 y 30 son
-- abundantes pero 5 y 28 no lo son.
-- 
-- Definir la funcion 
--    numeroAbundante :: Int -> Bool
-- tal que (numeroAbundante n) se verifica si n es un numero
-- abundante. Por ejemplo,  
--    numeroAbundante 5  == False
--    numeroAbundante 12 == True
--    numeroAbundante 28 == False
--    numeroAbundante 30 == True
-- ---------------------------------------------------------------------

numeroAbundante :: Int -> Bool
numeroAbundante n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la funcion  
--    numerosAbundantesMenores :: Int -> [Int]
-- tal que (numerosAbundantesMenores n) es la lista de numeros
-- abundantes menores o iguales que n. Por ejemplo,
--    numerosAbundantesMenores 50  ==  [12,18,20,24,30,36,40,42,48]
-- ---------------------------------------------------------------------

numerosAbundantesMenores :: Int -> [Int]
numerosAbundantesMenores n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Definir la funcion 
--    todosPares :: Int -> Bool
-- tal que (todosPares n) se verifica si todos los numeros abundantes
-- menores o iguales que n son pares. Por ejemplo,
--    todosPares 10    ==  True
--    todosPares 100   ==  True
--    todosPares 1000  ==  False
-- ---------------------------------------------------------------------

todosPares :: Int -> Bool
todosPares n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.4. Definir la constante 
--    primerAbundanteImpar :: Int
-- que calcule el primer numero natural abundante impar. Determinar el
-- valor de dicho numero.
-- ---------------------------------------------------------------------

primerAbundanteImpar :: Int
primerAbundanteImpar = undefined

-- Su calculo es
--    ghci> primerAbundanteImpar
--    945

-- ---------------------------------------------------------------------
-- Ejercicio 6 (Problema 1 del proyecto Euler) Definir la funcion 
--    euler1 :: Int -> Int
-- tal que (euler1 n) es la suma de todos los multiplos de 3 o 5 menores
-- que n. Por ejemplo,
--    euler1 10  ==  23
-- 
-- Calcular la suma de todos los multiplos de 3 o 5 menores que 1000.
-- ---------------------------------------------------------------------

euler1 :: Int -> Int
euler1 n = undefined

-- Calculo:

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la funcion 
--    circulo :: Int -> Int
-- tal que (circulo n) es el la cantidad de pares de numeros naturales
-- (x,y) que se encuentran dentro del circulo de radio n. Por ejemplo, 
--    circulo 3  ==  9
--    circulo 4  ==  15
--    circulo 5  ==  22
-- ---------------------------------------------------------------------

circulo :: Int -> Int
circulo n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Definir la funcion 
--    aproxE :: Double -> [Double]
-- tal que (aproXE n) es la lista cuyos elementos son los terminos de la
-- sucesion (1+1/m)**m desde 1 hasta n. Por ejemplo, 
--    aproxE 1 == [2.0]
--    aproxE 4 == [2.0,2.25,2.37037037037037,2.44140625]
-- ---------------------------------------------------------------------

aproxE :: Double -> [Double]
aproxE n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Cual es el limite de la sucesion (1+1/m)**m ?
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 8.3. Definir la funcion 
--    errorAproxE :: Double -> Double
-- tal que (errorE x) es el menor numero de terminos de la sucesion
-- (1+1/m)**m necesarios para obtener su limite con un error menor que
-- x. Por ejemplo, 
--    errorAproxE 0.1    ==  13.0
--    errorAproxE 0.01   ==  135.0
--    errorAproxE 0.001  ==  1359.0
-- Indicacion: En Haskell, e se calcula como (exp 1).
-- ---------------------------------------------------------------------

errorAproxE :: Double -> Double
errorAproxE x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir la funcion
--    aproxLimSeno :: Double -> [Double]
-- tal que (aproxLimSeno n) es la lista cuyos elementos son los terminos
-- de la sucesion  
--    sen(1/m) 
--    --------
--      1/m 
-- desde 1 hasta n. Por ejemplo,
--    aproxLimSeno 1 == [0.8414709848078965]
--    aproxLimSeno 2 == [0.8414709848078965,0.958851077208406]
-- ---------------------------------------------------------------------

aproxLimSeno :: Double -> [Double]
aproxLimSeno n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Cual es el limite de la sucesion sen(1/m)/(1/m) ?
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Definir la funcion 
--    errorLimSeno :: Double -> Double
-- tal que (errorLimSeno x) es el menor numero de terminos de la sucesion 
-- sen(1/m)/(1/m) necesarios para obtener su limite con un error menor
-- que x. Por ejemplo, 
--    errorLimSeno 0.1     ==   2.0
--    errorLimSeno 0.01    ==   5.0
--    errorLimSeno 0.001   ==  13.0
--    errorLimSeno 0.0001  ==  41.0
-- ---------------------------------------------------------------------

errorLimSeno :: Double -> Double
errorLimSeno x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10.1. Definir la funcion 
--    calculaPi :: Double -> Double
-- tal que (calculaPi n) es la aproximacion del numero pi calculada
-- mediante la expresion 
--    4*(1 - 1/3 + 1/5 - 1/7 + ...+ (-1)**n/(2*n+1))
-- Por ejemplo,
--    calculaPi 3    ==  2.8952380952380956
--    calculaPi 300  ==  3.1449149035588526
-- ---------------------------------------------------------------------

calculaPi :: Double -> Double
calculaPi n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10.2. Definir la funcion 
--    errorPi :: Double -> Double
-- tal que (errorPi x) es el menor numero de terminos de la serie
--    4*(1 - 1/3 + 1/5 - 1/7 + ...+ (-1)**n/(2*n+1))
-- necesarios para obtener pi con un error menor que x. Por ejemplo,
--    errorPi 0.1    ==    9.0
--    errorPi 0.01   ==   99.0
--    errorPi 0.001  ==  999.0
-- ---------------------------------------------------------------------

errorPi :: Double -> Double
errorPi x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Una terna (x,y,z) de enteros positivos es pitagorica
-- si x^2 + y^2 = z^2. 
-- 
-- Definir, por comprensi�n, la funci�n 
--    pitagoricas :: Int -> [(Int,Int,Int)]
-- tal que (pitagoricas n) es la lista de todas las ternas pitag�ricas
-- cuyas componentes est�n entre 1 y n. Por ejemplo, 
--    pitagoricas 10  ==  [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-- ---------------------------------------------------------------------

pitagoricas :: Int -> [(Int,Int,Int)]
pitagoricas n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Definir la funci�n 
--    numeroDePares :: (Int,Int,Int) -> Int
-- tal que (numeroDePares t) es el n�mero de elementos pares de la terna
-- t. Por ejemplo,
--    numeroDePares (3,5,7)  ==  0
--    numeroDePares (3,6,7)  ==  1
--    numeroDePares (3,6,4)  ==  2
--    numeroDePares (4,6,4)  ==  3
-- ---------------------------------------------------------------------

numeroDePares :: (Int,Int,Int) -> Int
numeroDePares (x,y,z) = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11.3. Definir la funci�n
--    conjetura :: Int -> Bool
-- tal que (conjetura n) se verifica si todas las ternas pitag�ricas
-- cuyas componentes est�n entre 1 y n tiene un n�mero impar de n�meros
-- pares. Por ejemplo,
--    conjetura 10  ==  True
-- ---------------------------------------------------------------------

conjetura :: Int -> Bool
conjetura n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11.4. Demostrar la conjetura para todas las ternas
-- pitag�ricas. 
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 12.1. (Problema 9 del Proyecto Euler). Una terna pitag�rica
-- es una terna de n�meros naturales (a,b,c) tal que a<b<c y
-- a^2+b^2=c^2. Por ejemplo (3,4,5) es una terna pitag�rica. 
-- 
-- Definir la funci�n 
--    ternasPitagoricas :: Integer -> [[Integer]]
-- tal que (ternasPitagoricas x) es la lista de las ternas pitag�ricas
-- cuya suma es x. Por ejemplo,
--    ternasPitagoricas 12  ==  [(3,4,5)]
--    ternasPitagoricas 60  ==  [(10,24,26),(15,20,25)]
-- ---------------------------------------------------------------------

ternasPitagoricas :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12.2. Definir la constante 
--    euler9 :: Integer
-- tal que euler9 es producto abc donde (a,b,c) es la �nica terna
-- pitag�rica tal que a+b+c=1000.  
--
-- Calcular el valor de euler9.
-- ---------------------------------------------------------------------

euler9 :: Integer
euler9 = undefined

-- El c�lculo del valor de euler9 es

-- ---------------------------------------------------------------------
-- Ejercicio 13. El producto escalar de dos listas de enteros xs y ys de
-- longitud n viene dado por la suma de los productos de los elementos
-- correspondientes. 
-- 
-- Definir por comprensi�n la funci�n 
--    productoEscalar :: [Int] -> [Int] -> Int
-- tal que (productoEscalar xs ys) es el producto escalar de las listas
-- xs e ys. Por ejemplo,
--    productoEscalar [1,2,3] [4,5,6]  ==  32
-- ---------------------------------------------------------------------

productoEscalar :: [Int] -> [Int] -> Int
productoEscalar xs ys = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir, por comprensi�n, la funci�n
--    sumaConsecutivos :: [Int] -> [Int]
-- tal que (sumaConsecutivos xs) es la suma de los pares de elementos
-- consecutivos de la lista xs. Por ejemplo,
--    sumaConsecutivos [3,1,5,2]  ==  [4,6,7]
--    sumaConsecutivos [3]        ==  []
-- ---------------------------------------------------------------------

sumaConsecutivos :: [Int] -> [Int]
sumaConsecutivos xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 15. Los polinomios pueden representarse de forma dispersa o
-- densa. Por ejemplo, el polinomio 6x^4-5x^2+4x-7 se puede representar
-- de forma dispersa por [6,0,-5,4,-7] y de forma densa por
-- [(4,6),(2,-5),(1,4),(0,-7)].  
-- 
-- Definir la funci�n 
--    densa :: [Int] -> [(Int,Int)]
-- tal que (densa xs) es la representaci�n densa del polinomio cuya
-- representaci�n dispersa es xs. Por ejemplo, 
--   densa [6,0,-5,4,-7]  ==  [(4,6),(2,-5),(1,4),(0,-7)]
--   densa [6,0,0,3,0,4]  ==  [(5,6),(2,3),(0,4)]
-- ---------------------------------------------------------------------

densa :: [Int] -> [(Int,Int)]
densa xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 16.0. La bases de datos sobre actividades de personas pueden
-- representarse mediante listas de elementos de la forma (a,b,c,d),
-- donde a es el nombre de la persona, b su actividad, c su fecha de
-- nacimiento y d la de su fallecimiento. Un ejemplo es la siguiente que
-- usaremos a lo largo de este ejercicio,
-- ---------------------------------------------------------------------

personas :: [(String,String,Int,Int)]
personas = [("Cervantes","Literatura",1547,1616),
            ("Velazquez","Pintura",1599,1660),
            ("Picasso","Pintura",1881,1973),
            ("Beethoven","Musica",1770,1823),
            ("Poincare","Ciencia",1854,1912),
            ("Quevedo","Literatura",1580,1654),
            ("Goya","Pintura",1746,1828),
            ("Einstein","Ciencia",1879,1955),
            ("Mozart","Musica",1756,1791),
            ("Botticelli","Pintura",1445,1510),
            ("Borromini","Arquitectura",1599,1667),
            ("Bach","Musica",1685,1750)]

-- ---------------------------------------------------------------------
-- Ejercicio 16.1. Definir la funci�n
--    nombres :: [(String,String,Int,Int)] -> [String]
-- tal que (nombres bd) es la lista de los nombres de las personas de la
-- base de datos bd. Por ejemplo,  
--    ghci> nombres personas
--     ["Cervantes","Velazquez","Picasso","Beethoven","Poincare",
--      "Quevedo","Goya","Einstein","Mozart","Botticelli","Borromini","Bach"]
-- ---------------------------------------------------------------------

nombres :: [(String,String,Int,Int)] -> [String]
nombres bd = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 16.2. Definir la funci�n
--    musicos :: [(String,String,Int,Int)] -> [String]
-- tal que (musicos bd) es la lista de los nombres de los m�sicos de la
-- base de datos bd. Por ejemplo,  
--    musicos personas  ==  ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

musicos :: [(String,String,Int,Int)] -> [String]
musicos bd = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 16.3. Definir la funci�n 
--    seleccion :: [(String,String,Int,Int)] -> String -> [String]
-- tal que (seleccion bd m) es la lista de los nombres de las personas
-- de la base de datos bd cuya actividad es m. Por ejemplo,  
--    ghci> seleccion personas "Pintura"
--    ["Velazquez","Picasso","Goya","Botticelli"]
--    ghci> seleccion personas "Musica"
--    ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

seleccion :: [(String,String,Int,Int)] -> String -> [String]
seleccion bd m = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 16.4. Definir, usando el apartado anterior, la funci�n
--    musicos' :: [(String,String,Int,Int)] -> [String]
-- tal que (musicos' bd) es la lista de los nombres de los m�sicos de la
-- base de datos bd. Por ejemplo,   
--    ghci> musicos' personas
--    ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

musicos' :: [(String,String,Int,Int)] -> [String]
musicos' bd = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 16.5. Definir la funci�n 
--    vivas :: [(String,String,Int,Int)] -> Int -> [String]
-- tal que (vivas bd a) es la lista de los nombres de las personas de la
-- base de datos bd  que estaban vivas en el a�o a. Por ejemplo,  
--    ghci> vivas personas 1600
--    ["Cervantes","Velazquez","Quevedo","Borromini"]
-- ---------------------------------------------------------------------

vivas :: [(String,String,Int,Int)] -> Int -> [String]
vivas ps a = undefined

