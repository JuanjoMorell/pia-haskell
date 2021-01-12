--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.List
import Data.Char

-- EJERCICIO 1
-- Nombre, Profesion, fechaNacimiento, fechaMuerte
type Persona = (String, String, Int, Int)
type Personas = [Persona]

genios :: Personas
genios = [("Cervantes","Literatura",1547,1616),("Velazquez","Pintura",1599,1660),("Picasso","Pintura",1881,1973),("Beethoven","Musica",1770,1823),("Poincare","Ciencia",1854,1912),("Quevedo","Literatura",1580,1654),("Goya","Pintura",1746,1828),("Einstein","Ciencia",1879,1955),("Mozart","Musica",1756,1791),("Botticelli","Pintura",1445,1510),("Borromini","Arquitectura",1599,1667),("Bach","Musica",1685,1750),("Sheldon Lee Cooper","Ciencia",1980,2080)]

-- EJERCICIO 2
nombres :: Personas -> [String]
nombres ps = [nombre | (nombre,pro,fn,fm) <- ps]

-- EJERCICIO 3
seleccion :: Personas -> String -> [String]
seleccion ps p = [nombre | (nombre,pro,fn,fm) <- ps, pro == p]

-- EJERCICIO 4
vivas :: Personas -> Int -> [String]
vivas ps f = [nombre | (nombre,pro,fn,fm) <- ps, fm > f, f > fn]

-- EJERCICIO 5
getOficios :: Personas -> [String]
getOficios ps = [pro | (nombre,pro,fn,fm) <- ps]

personasConOficio :: Personas -> String -> Personas
personasConOficio ps o = [(nombre,pro,fn,fm) | (nombre,pro,fn,fm) <- ps, pro == o]

ordenaPorOficio :: Personas -> Personas
ordenaPorOficio ps = concat [(personasConOficio ps pro) | pro <- (sort(getOficios ps))]

-- EJERCICIO 6
agrupaPorOficio :: Personas -> [Personas]
agrupaPorOficio ps = [(personasConOficio ps pro) | pro <- (sort(getOficios ps))]

-- EJERCICIO 7

-- lines entrada
autor :: String -> Bool
autor xs = n /= 1 && n > 1
    where n = (length (words xs))

dividirProfesiones :: [String] -> [(String, [String])]
dividirProfesiones [] = []
dividirProfesiones (x:xs) = [(x, (takeWhile (autor) xs))] ++ (dividirProfesiones (dropWhile (autor) xs))

leerAutor :: String -> (String, Int, Int)
leerAutor s = (takeWhile (`notElem` " ") s, read n1, read n2 )
    where n1 = takeWhile isDigit (tail (dropWhile (`notElem` "(") s))
          n2 = takeWhile isDigit (tail (dropWhile (`notElem` "-") s))

guardarAutores :: [(String, [String])] -> Personas
guardarAutores [] = []
guardarAutores ((prof, xs):ps) = [(nom,prof,fn,fm) | (nom,fn,fm) <- (map leerAutor xs) ] ++ (guardarAutores ps)

imprimirAutores :: Personas -> String
imprimirAutores ps = concat [nombre ++ " (" ++ pro ++ ") [" ++ show fn ++ "-" ++ show fm ++ "]" ++ "\n" | (nombre,pro,fn,fm) <- ps]

main :: IO()
main = do putStr "Introduzca fichero de entrada: "
          cadena <- getLine
          fichero <- readFile cadena
          putStrLn (imprimirAutores (guardarAutores (dividirProfesiones (lines fichero))))

-- EJERCICIO 8
data Persona' = P (String, String, Int, Int)
    deriving(Eq)

instance Show Persona' where
    show (P (nombre,pro,fn,fm)) = nombre ++ " (" ++ pro ++ ", " ++ show fn ++ "-" ++ show fm ++ ")"

-- EJERCICIO 9
instance Ord Persona' where
    (P (nombre,pro,fn,fm)) <= (P (nombre',pro',fn',fm')) = fn <= fn'
    (P (nombre,pro,fn,fm)) > (P (nombre',pro',fn',fm')) = fn > fn'

-- EJERCICIO 10
data Persona'' a b = P' (a,a,b,b)

cientificos :: [Persona'' String Int] -> [String]
cientificos ps = [nom | (P' (nom,pro,fn,fm)) <- ps, pro == "Ciencia"]

-----------SHELDON
-- EJERCICIO 1
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

primo :: Int -> Bool
primo n = (length (divisores n)) == 2

-- EJERCICIO 2
primos :: [Int]
primos = [x | x <- [1..], primo x]

-- EJERCICIO 3
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

productoDigitos :: Int -> Int
productoDigitos n = product (digs n)

-- EJERCICIO 4
inverso :: Int -> Int
inverso x= read (reverse (show x))

-- EJERCICIO 5
getPrimo :: Int -> Int
getPrimo n = (!!) primos n

esPrimoSheldon :: Int -> Bool
esPrimoSheldon x = n > 0 && x == primos !! (n - 1) && inverso x == primos !! (inverso n - 1)
  where n = productoDigitos x

-- EJERCICIO 6
primoSheldon :: Int
primoSheldon = head [x | x <- [1..], esPrimoSheldon x]