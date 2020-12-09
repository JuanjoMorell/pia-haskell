--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-} 

-- Definicion de tipos para la base de datos de la biblioteca
type Persona = String
type Libro = String
type BD = [(Persona,Libro)]

miBD :: BD
miBD = [("Juan","Opiniones de un payaso"),("Luis","Ulises"),("Pedro","Los misterios de Madrid"),("Juan","1984")]

misEjem :: NumEjem
misEjem = [("Opiniones de un payaso",1),("Ulises",7),("Los misterios de Madrid",0),("1984",3)]

-- Dada una persona, obtener los libros que tiene en préstamo
libros :: BD -> Persona -> [Libro]
libros bd p = [y | (x,y) <- bd, x == p]

lectores :: BD -> Libro -> [Persona]
lectores bd l = [x | (x,y) <- bd, y == l]

prestado :: BD -> Libro -> Bool
prestado bd l = not (lecto == [])
    where lecto = lectores bd l

prestado' :: BD -> Libro -> Bool
prestado' bd = not.null.(lectores bd)

numPrestados :: BD -> Persona -> Int
numPrestados bd p = length (prestados) 
    where prestados = libros bd p

realizarPrestamo :: BD -> Persona -> Libro -> BD
realizarPrestamo bd p l = bd ++ [(p,l)]

devolverPrestamo :: BD -> Persona -> Libro -> BD
devolverPrestamo bd p l = [prestamo | prestamo <- bd, prestamo /= (p,l)]

type NumEjem = [(Libro,Int)]

disponibleLibro :: NumEjem -> Libro -> Bool
disponibleLibro ne l = not(null(ejemplares))
    where ejemplares = [y | (x,y) <- ne, x == l && y /= 0]

nuevoEjemplar :: NumEjem -> Libro -> NumEjem
nuevoEjemplar ne l = if (disponibleLibro ne l) then [(x, if x == l then y+1 else y) | (x,y) <- ne] else ne++[(l,1)]

devuelveEjemplar, extraeEjemplar :: NumEjem -> Libro -> NumEjem


--realizarPrestamo :: BD -> NumEjem -> Persona -> Libro -> (BD,NumEjem)


--devolverPrestamo :: BD -> NumEjem -> Persona -> Libro -> (BD,NumEjem)
