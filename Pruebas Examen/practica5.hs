--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- EJERCICIO 1
type Persona = String
type Libro = String
type BD = [(Persona,Libro)]
type NumEjem = [(Libro,Int)]

miBD :: BD
miBD=[("Juan","Opiniones de un payaso"),("Luis","Ulises"),("Pedro","Los misterios de Madrid"),("Juan","1984")]

misEjem :: NumEjem
misEjem = [("Opiniones de un payaso", 1), ("Ulises",7),("Los misterios de Madrid",0),("1984",3)] 

-- EJERCICIO 2
libros :: BD -> Persona -> [Libro]
libros bd per = [libro | (persona,libro) <- bd, persona == per]

lectores :: BD -> Libro -> [Persona]
lectores bd l = [persona | (persona,libro) <- bd, libro == l]

prestado :: BD -> Libro -> Bool
prestado bd l = length (lectores bd l) > 0

numPrestados :: BD -> Persona -> Int
numPrestados bd p = length(libros bd p)

-- EJERCICIO 3
realizarPrestamo :: BD -> Persona -> Libro -> BD
realizarPrestamo bd p l = bd ++ [(p,l)]

devolverPrestamo :: BD -> Persona -> Libro -> BD
devolverPrestamo bd p l = [prestamo | prestamo <- bd, prestamo /= (p,l) ]