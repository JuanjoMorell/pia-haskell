--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

---------------------------------------------------------------
--------------------------------------------------------PARTE 1
---------------------------------------------------------------

type Persona = String
type Libro = String
type BD = [(Persona, Libro)]

miBD :: BD
miBD=[("Juan","Opiniones de un payaso"),("Luis","Ulises"),("Pedro","Los misterios de Madrid"),("Juan","1984")]

libros :: BD -> Persona -> [Libro]
libros bd persona = [libro | (x, libro) <- bd, x == persona]

lectores :: BD -> Libro -> [Persona]
lectores bd libro = [persona | (persona, x) <- bd, x == libro]

prestado :: BD -> Libro -> Bool
prestado bd libro = length (lectores bd libro) /= 0 

numPrestados :: BD -> Persona -> Int
numPrestados bd persona = length (libros bd persona)

realizarPrestamo :: BD -> Persona -> Libro -> BD
realizarPrestamo bd persona libro = bd ++ [(persona, libro)]

devolverPrestamo :: BD -> Persona -> Libro -> BD
devolverPrestamo bd persona libro = [prestamo | prestamo <- bd, prestamo /= (persona, libro)]

type NumEjem = [(Libro,Int)]

misEjem :: NumEjem
misEjem = [("Opiniones de un payaso", 1), ("Ulises",7),("Los misterios de Madrid",0),("1984",3)] 

disponibleLibro :: NumEjem -> Libro -> Bool
disponibleLibro ne libro = length[ejem | (l, ejem) <- ne, l == libro] >= 1

catalogadoLibro :: NumEjem -> Libro -> Bool
catalogadoLibro ne libro = not (null [l | (l, numl) <- ne, l == libro])

nuevoEjemplar :: NumEjem -> Libro -> NumEjem
nuevoEjemplar ne libro = if (catalogadoLibro ne libro) then [(x,if x==libro then y+1 else y)| (x,y) <- ne] else ne++[(libro,1)]

devuelveEjemplar, extraeEjemplar :: NumEjem -> Libro -> NumEjem
devuelveEjemplar ne libro = [(x,if x==libro then y+1 else y)| (x,y)<-ne]

extraeEjemplar ne libro = if disponibleLibro ne libro then [(x,y-1)| (x,y)<-ne, x==libro] else error("No hay ejemplares")

realizarPrestamo2 :: BD -> NumEjem -> Persona -> Libro -> (BD,NumEjem)
realizarPrestamo2 bd ne p l = if disponibleLibro ne l then (realizarPrestamo bd p l, extraeEjemplar ne l) else error("No hay ejemplares")

devolverPrestamo2 :: BD -> NumEjem -> Persona -> Libro -> (BD, NumEjem)
devolverPrestamo2 bd ne p l = if prestado bd l then (devolverPrestamo bd p l, devuelveEjemplar ne l) else (bd,ne)

