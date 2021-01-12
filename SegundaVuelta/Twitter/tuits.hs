--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import ListasEspeciales
import Data.List
import Data.Char

-- EJERCICIO 1
type Dia = Int
type Mes = Int
type Anyo = Int
type Fecha = (Dia,Mes,Anyo)
type Identidad = ListaEspecial Char 
type Mensaje = ListaEspecial Char
type Hashtag = ListaEspecial Char

-- EJERCICIO 2
esFecha :: Fecha -> Bool
esFecha (d,m,a) = (d >= 1) && (d <= 31) && (m >= 1) && (m < 12) && (a >= 1) && (a <= 9999)

esIdentidad :: Identidad -> Bool
esIdentidad ls = (head ls) == '@'

esHashtag :: Hashtag -> Bool
esHashtag ls = (head ls) == '#'

-- EJERCICIO 3
data Tuit = T Fecha Identidad Mensaje

type Tuits = [Tuit]

esTuit :: Tuit -> Bool
esTuit (T f id msj) = esFecha f && esIdentidad id && esCorta msj

instance Show Tuit where
    show (T (d,m,a) id msj)
            | esTuit (T (d,m,a) id msj) = show id ++ " (" ++ show d ++ "/" ++ show m ++ "/" ++ show a ++ "): " ++ show msj ++ "\n"
            | esLarga msj = show id ++ " (" ++ show d ++ "/" ++ show m ++ "/" ++ show a ++ "): " ++ show (recortarLista msj) ++ "\n"
            | otherwise = error "Fecha o identidad no validos"

-- EJERCICIO 4
identidades :: Tuits -> [Identidad]
identidades ts = [id | (T f id msj) <- ts]

-- EJERCICIO 5
mensajes :: Tuits -> [Mensaje]
mensajes ts = [msj | (T f id msj) <- ts]

-- EJERCICIO 6
qSort :: Ord a => [a]-> [a]
qSort [] = []
qSort (x:xs) = qSort[y | y<-xs,y<=x]++[x]++qSort[y | y<-xs,y>x]  

masActivo :: Tuits -> Identidad
masActivo ts = (fst.head.(filter ((==maxId ts).snd)).na) ts
            where maxId = maximum.snd.unzip.na
                  na = (map longEid).group.qSort.identidades 
                  longEid ids = (head ids,length ids)

-- EJERCICIO 7
espacios :: String
espacios = ['\t', '\n', '\r', '\f', '\v', ',', '.', ';', ':', ' ']

type Separadores = String

words' :: Separadores -> String -> [String]
words' sp [] = []
words' sp st = takeWhile (`notElem` sp) st' : words' sp (dropWhile (`notElem` sp) st')
    where st' = quitaEspInFin st
          quitaEspInFin = dropWhile (`elem` sp).reverse.dropWhile (`elem` sp).reverse

buscaHashtag :: Mensaje -> [Hashtag]
buscaHashtag msj = [p | p <- (words' espacios msj), esHashtag p]

-- EJERCICIO 8
todosLosHashtags :: [Mensaje] -> [Hashtag]
todosLosHashtags msjs = borrarRepes (concat (map buscaHashtag msjs))
    where borrarRepes [] = []
          borrarRepes (h:hs) = if elem h' hs' then hs else (h'):(borrarRepes hs)
                where h' = map toLower h
                      hs' = map (map toLower) hs

-- EJERCICIO 9
tuitsConHashtag :: Tuits -> Hashtag -> Tuits
tuitsConHashtag ts hs = [(T f id msj) | (T f id msj) <- ts, elem hs (buscaHashtag msj)]

-- EJERCICIO 10
personasHashtag :: Tuits -> Hashtag -> [Identidad]
personasHashtag ts hs = borraRepetidos [id | (T f id msj) <- (tuitsConHashtag ts hs)]
    where borraRepetidos [] = []
          borraRepetidos (i:is) 
               | elem i is = borraRepetidos is
               | otherwise = i:borraRepetidos is 

-- EJERCICIO 11
tuitsConMismoHashtag :: Tuits -> [(Hashtag, Tuits)]
tuitsConMismoHashtag ts = [(hs, (tuitsConHashtag ts hs)) | hs <- (todosLosHashtags (mensajes ts)) ]

-- EJERCICIO 12
numeroDeHashtags :: Tuits -> [(Hashtag,Int)]
numeroDeHashtags = (map longitud).tuitsConMismoHashtag
                        where longitud (h,l) = (h,length l)

treningTopic :: Tuits -> Hashtag
treningTopic = fst.(foldr conMasCitas ("",0)).numeroDeHashtags 
                         where conMasCitas (h,n) (h',n')
                                 | n>n'= (h,n) 
                                 | otherwise = (h',n')

-- EJERCICIO 13
posterior :: Fecha -> Fecha -> Bool
posterior (d,m,a) (d',m',a') = (d < d') && (m <= m') && (a <= a')

tuitsDesde :: Fecha -> Tuits -> Tuits
tuitsDesde f ts = [(T f' id msj) | (T f' id msj) <- ts, (posterior f f')]

-- EJERCICIO 14
formatoFecha :: String -> Bool
formatoFecha [d1,d2,g1,m1,m2,g2,a1,a2,a3,a4] = 
  and (map isDigit [d1,d2,m1,m2,a1,a2,a3,a4])&&g1=='-' &&g2 =='-'
formatoFecha _ = False

leeFecha :: String -> (Dia,Mes,Anyo)
leeFecha = aTupla.map read.(words' ['-'])
              where aTupla [x,y,z] = (x,y,z)

leerTuits :: String -> Tuits
leerTuits st = [T (leeFecha f) i m | (f,i,m)<- zip3 listaF listaI listaM ]
                       where todo = filter (/="") (lines st)
                             listaF = filter formatoFecha todo
                             listaI = filter esIdentidad todo
                             listaM = todo \\ (listaI++listaF)

operaciones :: Tuits -> IO()
operaciones ts = do putStrLn "Seleccione operacion: "
                    putStrLn "(1) Imprimir todos los tuits"
                    putStrLn "(2) Imprimir todos los hashtags"
                    putStrLn "(3) Imprimir numero de apariciones de cada hashtag"
                    putStrLn "(9) Salir"
                    op <- getLine
                    case op of
                       "1" -> do putStrLn "TODOS LOS TUITS"
                                 operaciones ts
                       "2" -> do putStrLn "TODOS LOS HASHTAG"
                                 operaciones ts
                       "3" -> do putStrLn "APARICIONES DE CADA HASHTAG"
                                 putStrLn (masActivo ts)
                                 operaciones ts
                       "9" -> return()


main = do putStr "Introduzca fichero de entrada: "
          fEntrada <- getLine
          contenido <- readFile fEntrada
          let tuits = leerTuits contenido
          putStrLn "Fichero leido"
          operaciones tuits
          
                
