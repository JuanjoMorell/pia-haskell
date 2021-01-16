--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import MatricesR
import Data.List
import Data.Char

-- EJERCICIO 1
type Nombre = String
type Etiqueta = String
type FSet = [(Int,Float)]
type M = [(Etiqueta,FSet)]

data VLing = VL Nombre Int [Etiqueta] M

type Proposicion = (Nombre,Etiqueta)

data Regla = R Proposicion Proposicion

-- VARIABLES PARA HACER LOS EJEMPLOS
vl1 :: VLing
vl1 = (VL "X" 4 ["Grande","Pequena"] [("Pequena",[(1,1.0),(2,0.8),(3,0.7),(4,0.0)]),("Grande",[(1,0.0),(2,0.2),(3,0.7),(4,1.0)])])

vl2 :: VLing
vl2 = (VL "Y" 3 ["Pequena"] [("Pequena",[(1,1.0),(2,0.5),(3,0.1)])])

r1 :: Regla
r1 = (R ("X","Grande") ("Y", "Pequena"))

-- EJERCICIO 2
impValores :: Int -> String
impValores n = (concat [show x ++ "," | x <- [1..(n-1)]]) ++ (show n)

impEtiq :: [Etiqueta] -> String
impEtiq xs = concat [x ++ "," | x <- (init xs)] ++ (last xs)

instance Show VLing where
    show (VL nombre n xs m) = nombre ++ " toma valores en {" ++ impValores n ++ "} y puede ser {" ++ impEtiq xs ++ "}"

instance Show Regla where
    show (R (nombre,etiq) (nombre2,etiq2)) = nombre ++ " es " ++ etiq ++ " => " ++ nombre2 ++ " es " ++ etiq2

-- EJERCICIO 3
buscaM :: Nombre -> [VLing] -> M
buscaM nombre vls = head [m | (VL nom n xs m) <- vls, nom == nombre]

-- EJERCICIO 4
buscaSet :: Etiqueta -> M -> FSet
buscaSet etiq m = head [fset | (etiq2,fset) <- m, etiq2 == etiq]

-- EJERCICIO 5
obtenerConjuntoBorroso :: Proposicion -> [VLing] -> FSet
obtenerConjuntoBorroso (nombre,etiq) vls = buscaSet etiq (buscaM nombre vls)

valoresFSet :: FSet -> [Float]
valoresFSet fset = [f | (n,f) <- fset]

creaMatrizRelacion :: (Float->Float->Float) -> [VLing] -> Regla -> Matriz Float
creaMatrizRelacion f vls (R p1 p2) = productoCartesianoF f valoresP1 valoresP2
        where valoresP1 = valoresFSet (obtenerConjuntoBorroso p1 vls)
              valoresP2 = valoresFSet (obtenerConjuntoBorroso p2 vls)

-- EJERCICIO 6
creaRelacionMandami, creaRelacionLarsen :: [VLing] -> Regla -> Matriz Float
creaRelacionMandami vls r = creaMatrizRelacion min vls r
creaRelacionLarsen vls r = creaMatrizRelacion (*) vls r

-- EJERCICIO 7
trasnformarFSet :: [Float] -> FSet
trasnformarFSet xs = [(n,x) | (n,x) <- (zip [1..(length xs)] xs)]

modusPonensG :: (Float->Float->Float) -> [VLing] -> Proposicion -> Regla -> FSet
modusPonensG f vls prop r = trasnformarFSet (combinaMatrices max f valoresProp mRelacion)
    where mRelacion = creaMatrizRelacion f vls r
          valoresProp = valoresFSet (obtenerConjuntoBorroso prop vls)

-- EJERCICIO 8
leerProposicion :: String -> Proposicion
leerProposicion xs = (head palabras, last palabras)
    where palabras = words xs

leerRegla :: String -> Regla
leerRegla xs = (R prop1 prop2)
    where prop1 = leerProposicion (init(takeWhile (/= '=') xs))
          prop2 = leerProposicion (tail(dropWhile (/= '>') xs))

-- FALTA POR LEER EL FICHERO CON LAS VARIABLES LINGUISTICAS
separarVLS :: String -> [String]
separarVLS [] = []
separarVLS st = [unlines(takeWhile (/= "") lineas)] ++ separarVLS (unlines (dropWhile (/= "") lineas))
    where lineas = lines st

readEtiquetas :: [String] -> [Etiqueta]
readEtiquetas xs = [head(words x) | x <- xs]

leerFSet :: String -> FSet
leerFSet [] = []
leerFSet st = [(read(head(take 2 numeros)), read(last(take 2 numeros)))] ++ leerFSet (unwords(drop 2 numeros))
    where numeros = words st

readM :: [String] -> M
readM xs = [(head(words x), leerFSet (unwords(tail(words x)))) | x <- xs, x /= ""]

leerVL :: String -> VLing
leerVL st = (VL nombre n etiq m)
    where nombre = head lineas
          n = read (head(tail lineas))
          etiq = readEtiquetas (drop 2 lineas)
          m = readM (drop 2 lineas)
          lineas = lines st

leerVLS :: String -> [VLing]
leerVLS st = [leerVL x | x <- vls]
    where vls = separarVLS st

main :: IO()
main = do putStrLn "BIENVENIDO A INFERENCIA BORROSA"
          putStr "Introduzca fichero con variables linguisticas: "
          fVLS <- getLine
          contenidoVLS <- readFile fVLS
          let vls = init(leerVLS contenidoVLS)
          putStr "Introduzca fichero con una regla: "
          fR <- getLine
          contenidoR <- readFile fR
          let regla = leerRegla contenidoR
          putStrLn (show regla)
          putStr "Introduzca fichero con una proposicion: "
          fProp <- getLine
          contenidoProp <- readFile fProp
          let prop = leerProposicion contenidoProp
          putStrLn (show prop)
          putStrLn "VARIABLES LEIDAS"
          let valor = modusPonensG min vls prop regla
          putStrLn ("Modus ponens = " ++ show valor)


