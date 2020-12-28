--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Listas
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
esFecha (x,y,z) = (1 <= x && x <= 31) && (1 <= y && y <= 12) && (1 <= z && z <= 9999)

esIdentidad :: Identidad -> Bool
esIdentidad id = head id == '@'

esHashtag :: Hashtag -> Bool
esHashtag ht = head ht == '#'

-- EJERCICIO 3
data Tuit = T Fecha Identidad Mensaje
type Tuits = [Tuit]

isTuit :: Tuit -> Bool
isTuit (T f id msj) = esFecha f && esIdentidad id && esCorta msj

instance Show Tuit where
    show (T (d,m,a) id msj)
        | isTuit (T (d,m,a) id msj) = id ++ " (" ++ show d ++ "/" ++ show m ++ "/" ++ show a ++ "): " ++ msj ++ "\n"
        | esLarga msj = id ++ " (" ++ show d ++ "/" ++ show m ++ "/" ++ show a ++ "): " ++ recortaLista msj ++ "\n"
        | otherwise = error "Fecha o identidad no correcta"

-- EJERCICIO 4
identidades :: Tuits -> [Identidad]
identidades ts = [id | (T f id msj) <- ts]

-- EJERCICIO 5
mensajes :: Tuits -> [Mensaje]
mensajes ts = [msj | (T f id msj) <- ts]

-- EJERCICIO 6
qSort :: Ord a => [a]-> [a]
qSort [] = []
qSort (x:xs) = qSort[y | y<-xs,y<=x] ++ [x] ++ qSort[y | y<-xs,y>x]  

masActivo :: Tuits -> Identidad
masActivo ts = (fst.head.(filter ((==maxId ts).snd)).na) ts
            where maxId = maximum.snd.unzip.na
                  na = (map longEid).group.qSort.identidades 
                  longEid ids = (head ids,length ids)

-- EJERCICIO 7
espacios :: String
espacios = ['\t', '\n', '\r', '\f', '\v', ',', '.', ';', ':', ' ']

type Separadores = String

words' ::  Separadores -> String -> [String]
words' sp [] = []
words' sp  cs = (takeWhile (`notElem` sp)) cs' : words' sp (dropWhile (`notElem` sp) cs')
             where cs' = quitaEspInFin cs
                   quitaEspInFin = dropWhile (`elem` sp).reverse.dropWhile (`elem` sp).reverse

buscaHashtag :: Mensaje -> [Hashtag]
buscaHashtag m = [h | h <- (words' espacios m), esHashtag h]

-- EJERCICIO 8
todosLosHashtag :: [Mensaje] -> [Hashtag]
todosLosHashtag = borraRepetidos.concat.(map buscaHashtag)
    where borraRepetidos [] = []
          borraRepetidos (h:hs)
                        | elem (map toLower h) hsM = borraRepetidos hs
                        | otherwise = (map toLower h):borraRepetidos hs
                        where hsM = minusc' hs
                              minusc' = map minusc
                              minusc = map toLower

-- EJERCICIO 9
tuitsConHashtag :: Tuits -> Hashtag -> Tuits
tuitsConHashtag ts ht = [(T f id msj) | (T f id msj) <- ts,elem (map toLower ht) (buscaHashtag (map toLower msj))]

-- EJERCICIO 10
personasHashtag :: Tuits -> Hashtag -> [Identidad]
personasHashtag ts ht = borrarRepetidos [id | (T f id msj) <- (tuitsConHashtag ts ht)]
        where borrarRepetidos [] = []
              borraRepetidos (id:ids)
                            | elem id ids = borrarRepetidos ids
                            | otherwise = id : borrarRepetidos ids

-- EJERCICIO 11
tuitsConMismoHashtag :: Tuits -> [(Hashtag,Tuits)]
tuitsConMismoHashtag ts = [(ht, tuitsConHashtag ts ht) | ht <- hs]
    where hs = todosLosHashtag (mensajes ts)

-- EJERCICIO 12
numeroDeHashtag :: Tuits -> [(Hashtag,Int)]
numeroDeHashtag ts = map longitud (tuitsConMismoHashtag ts)
    where longitud (h,l) = (h, length l)

trendingTopic :: Tuits -> Hashtag
trendingTopic ts = fst (foldr conMasCitas ("",0) (numeroDeHashtag ts))
    where conMasCitas (h,l) (h',l')
                    | l > l' = (h,l)
                    | otherwise = (h',l')

-- EJERCICIO 13
fechaposterior :: Fecha -> Fecha -> Bool
fechaposterior (d,m,a) (d',m',a')
        | d <= d' = True
        | d > d' = False
        | m <= m' = True
        | m > m' = False
        | a <= a' = True
        | a > a' = False
        | otherwise = False

tuitsDesde :: Fecha -> Tuits -> Tuits
tuitsDesde f ts = [(T f' id msj) | (T f' id msj) <- ts, fechaposterior f f']

-- EJERCICIO 14
formatoFecha :: String -> Bool
formatoFecha [d1,d2,g1,m1,m2,g2,a1,a2,a3,a4] = 
  and (map isDigit [d1,d2,m1,m2,a1,a2,a3,a4])&&g1=='-' &&g2 =='-'
formatoFecha _ = False

leeFecha :: String -> (Dia,Mes,Anyo)
leeFecha = aTupla.map read.(words' ['-'])
              where aTupla [x,y,z] = (x,y,z)

separaTuits :: String -> Tuits
separaTuits st = [T (leeFecha f) i m | (f,i,m)<- zip3 listaF listaI listaM ]
                       where todo = filter (/="") (lines st)
                             listaF = filter formatoFecha todo
                             listaI = filter esIdentidad todo
                             listaM = todo \\ (listaI++listaF)

imprimeTuits :: Tuits -> IO ()
imprimeTuits [] = return ()
imprimeTuits (t:ts) = print t >> imprimeTuits ts

imprimeHashtag :: [ListaEspecial Char] -> IO ()
imprimeHashtag [] = return ()
imprimeHashtag (h:hs) = putStrLn h >> imprimeHashtag hs

imprimeHashtagConTuits :: [(Hashtag,Tuits)] -> IO ()
imprimeHashtagConTuits [] = return ()
imprimeHashtagConTuits ((h,ts):htss) = putStrLn h >> imprimeTuits ts >> imprimeHashtagConTuits htss

imprimeHashtagConNum :: [(Hashtag,Int)] -> IO ()
imprimeHashtagConNum [] = return ()
imprimeHashtagConNum ((h,n):hns) =   putStr (h ++ " --> ") >>  print n  >> imprimeHashtagConNum hns


main :: IO()
main = do putStr "Introduce el nombre del fichero de entrada -> "
          fichero <- getLine
          tuitsString <- readFile fichero
          let tuits = separaTuits tuitsString
          putStrLn "Elige una opcion:"
          putStrLn "(1) Imprimir todos los tuits"
          putStrLn "(2) Imprimir todos los hashtag"
          putStrLn "(3) Imprimir numero de apariciones de cada hashtag"
          putStrLn "(4) Imprimir los tuits en los que aparece un hashtag"
          putStrLn "(5) Imprimir los usuarios que comparten un cierto hashtag"
          putStrLn "(6) Imprimir el usuario mas activo"
          putStrLn "(7) Imprimir los tuits publicados a partir de una determinada fecha"
          putStrLn "(8) Imprimir el trending topic"
          putStrLn "(9) Salir"
          opcion <- getLine
          case opcion of 
              "1" -> do putStrLn "Todos los tuits:"
                        imprimeTuits tuits
                        main
              "2" -> do putStrLn "Todos los hashtag:"
                        imprimeHashtag (todosLosHashtag (mensajes tuits))
                        main
              "3" -> do putStrLn "Numero de apariciones de cada hashtag:"
                        imprimeHashtagConNum (numeroDeHashtag tuits)
                        main
              "4" -> do putStrLn "Tuits en los que aparece un hashtag:"
                        putStr "Introduzca hashtag: "
                        ht <- getLine
                        imprimeTuits (tuitsConHashtag tuits ht) 
                        main
              "5" -> do putStrLn "Usuarios que comparten un cierto hashtag:"
                        putStr "Introduzca hashtag: "
                        ht <- getLine
                        imprimeHashtag (personasHashtag tuits ht)
                        main
              "6" -> do putStrLn "Usuario mas activo:"
                        putStrLn (masActivo tuits)
                        main
              "7" -> do putStrLn "Tuits publicados a partir de una determinada fecha"
                        putStr "Introduzca fecha: "
                        f <- getLine
                        imprimeTuits (tuitsDesde (leeFecha f) tuits)
                        main
              "8" -> do putStrLn "Trending topic:"
                        putStrLn (trendingTopic tuits)
                        main
              "9" -> return()