--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Char
import Data.List
import ListasEspeciales

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
esFecha (d,m,a) = 1 <= d && d <= 31 && 1 <= m && m <= 12 && 1 <= a && a <= 9999

esIdentidad :: Identidad -> Bool
esIdentidad = comienzaPor '@'

esHashtag :: Hashtag -> Bool
esHashtag = comienzaPor '#'

-- EJERCICIO 3
data Tuit = T Fecha Identidad Mensaje

instance Show Tuit where
    show (T (d,m,a) id msj)
        | esIdentidad id && esFecha (d,m,a) = show id ++ " (" ++ show d ++ "/" ++ show m ++ "/" ++ show a ++ "): " ++ show (recortaLista msj)
        | otherwise = error "mal id o fecha"

type Tuits = [Tuit]

-- EJERCICIO 4
identidades :: Tuits -> [Identidad]
identidades ts = [id | (T f id msj) <- ts]

-- EJERCICIO 5
mensajes :: Tuits -> [Mensaje]
mensajes ts = [msj | (T f id msj) <- ts]

-- EJERCICIO 6
identidadesUnicas :: [Identidad] -> [Identidad] -> [Identidad]
identidadesUnicas [] xs = xs
identidadesUnicas (id:ids) xs = if elem id xs then xs ++ (identidadesUnicas ids xs) else xs ++ [id] ++(identidadesUnicas ids xs)

conteoIdentidad :: [(Identidad,Int)] -> Identidad -> [(Identidad,Int)]
conteoIdentidad [] idd = []
conteoIdentidad ((id,i):xss) idd = if id == idd then [(id,i+1)] ++ (conteoIdentidad xss idd) else [(id,i)] ++ (conteoIdentidad xss idd)

conteoIdentidades :: [(Identidad,Int)] -> [Identidad] -> [(Identidad,Int)]
conteoIdentidades xss [] = xss
conteoIdentidades xss (id:ids) = conteoIdentidades (conteoIdentidad xss id) ids

buscarMasActivo :: [(Identidad,Int)] -> (Identidad,Int) -> Identidad
buscarMasActivo [] (id,n) = id
buscarMasActivo ((id1,n1):xs) (id2,n2) = if n1 > n2 then buscarMasActivo xs (id1,n1) else buscarMasActivo xs (id2,n2)

masActivo :: Tuits -> Identidad
masActivo ts = buscarMasActivo (conteoIdentidades conteo ids) (head ids, 0)
    where ids = identidades ts
          conteo = zip (identidadesUnicas ids []) [0..]
          total = conteoIdentidad conteo 

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
hashtagUnicos :: [Hashtag] -> [Hashtag] -> [Hashtag]
hashtagUnicos [] xs = xs
hashtagUnicos (h:hs) xs = if elem h xs then xs ++ (hashtagUnicos hs xs) else xs ++ [h] ++(hashtagUnicos hs xs)

todosLosHashtag :: [Mensaje] -> [Hashtag]
todosLosHashtag msjs = hashtagUnicos (concat [(buscaHashtag (map toLower x)) | x <- msjs]) []

-- EJERCICIO 9
tuitsConHashtag :: Tuits -> Hashtag -> Tuits
tuitsConHashtag ts h = [(T f id msj) | (T f id msj) <- ts, elem h' (buscaHashtag (map toLower msj))]
    where h' = map toLower h

-- EJERCICIO 10
personasHashtag :: Tuits -> Hashtag -> [Identidad]
personasHashtag ts h = [id | (T f id msj) <- (tuitsConHashtag ts h)]

-- EJERCICIO 11
tuitsConMismoHashtag :: Tuits -> [(Hashtag,Tuits)]
tuitsConMismoHashtag ts = [(h,(tuitsConHashtag ts h)) | h <-  todosLosHashtag msjs]
    where msjs = [msj | (T f id msj) <- ts]

numeroDeHashtags :: Tuits -> [(Hashtag,Int)]
numeroDeHashtags ts = [(h,length tss) | (h,tss) <-  tuitsConMismoHashtag ts]

-- EJERCICIO 12
masCitado :: [(Hashtag,Tuits)] -> (Hashtag, Int) -> (Hashtag, Int)
masCitado [] (h,n) = (h,n)
masCitado ((h,ts):xs) (ha,na) = if (length ts) > na then masCitado xs (h,(length ts)) else masCitado xs (ha,na)

trendingTopic :: Tuits -> Hashtag
trendingTopic ts = h
    where (h,n) = masCitado (tuitsConMismoHashtag ts) ([],0) 

-- EJERCICIO 13
anteriorFecha :: Fecha -> Fecha -> Bool 
anteriorFecha (d1,m1,a1) (d2,m2,a2) 
     | a1 < a2 = True
     | a1 > a2 = False
     | m1 < m2 = True
     | m1 > m2 = False
     | d1 < d2 = True
     | d1 > d2 = False
     | otherwise = True 

tuitsDesde :: Fecha -> Tuits -> Tuits
tuitsDesde fecha  ts = [T f i m | (T f i m) <- ts, anteriorFecha fecha f]

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

imprimeHoIs :: [ListaEspecial Char] -> IO ()
imprimeHoIs [] = return ()
imprimeHoIs (h:hs) = putStrLn h >> imprimeHoIs hs

imprimeHashtagConTuits :: [(Hashtag,Tuits)] -> IO ()
imprimeHashtagConTuits [] = return ()
imprimeHashtagConTuits ((h,ts):htss) = putStrLn h >> imprimeTuits ts >> imprimeHashtagConTuits htss

imprimeHashtagConNum :: [(Hashtag,Int)] -> IO ()
imprimeHashtagConNum [] = return ()
imprimeHashtagConNum ((h,n):hns) =   putStr (h ++ " --> ") >>  print n  >> imprimeHashtagConNum hns

main :: IO ()
main = do putStr "Introduce el nombre del fichero con los tuits: "
          nombreF <- getLine
          tuitsString <- readFile nombreF
          let ts = separaTuits tuitsString
          putStrLn "Elige opcion:"
          putStrLn "0.-Imprimir todos los tuits."
          putStrLn "1.-Imprimir todos los hashtags."
          putStrLn "2.-Imprimir todos los hashtags con los tuits en los que aparecen."
          putStrLn "3.-Imprimir nuumero de apariciones de cada hashtag."
          putStrLn "4.-Imprimir los tuits en los que aparece un hashtag."
          putStrLn "5.-Imprimir los usuarios que comparten un cierto hashtag."
          putStrLn "6.-Imprimir el usuario mas activo."
          putStrLn "7.-Imprimir los tuits desde una fecha."
          putStrLn "8.-Imprimir el trending topic."
          putStrLn "9.-Salir"

          o <- getLine
          case o of
           "0" -> do putStrLn "\nTODOS LOS TUITS:"
                     imprimeTuits ts
                     main
           "1" -> do putStrLn "\nTODOS LOS HASHTAG:"
                     imprimeHoIs (todosLosHashtag (mensajes ts))
                     main
           "2" -> do putStrLn "\nTODOS LOS HASHTAG CON SUS TUITS:"
                     imprimeHashtagConTuits (tuitsConMismoHashtag ts)
                     main
           "3" -> do putStrLn "\nAPARICION DE CADA HASHTAG:"
                     imprimeHashtagConNum (numeroDeHashtags ts)
                     main
           "4" -> do putStr "ELIGE HASHTAG: "
                     h <- getLine            
                     putStrLn ("TUITS CON HASHTAG "++h++":")
                     imprimeTuits (tuitsConHashtag ts h) 
                     main
           "5" -> do putStr "ELIGE HASHTAG: "
                     h <- getLine            
                     putStrLn ("USUARIOS INTERESADOS EN HASHTAG "++h++":")
                     imprimeHoIs (personasHashtag ts h) 
                     main
           "6" -> do putStrLn "\nUSUARIO MAS ACTIVO:"
                     putStrLn (masActivo ts)
                     main
           "7" -> do putStrLn "\nELIGE FECHA (dd-mm-aaaa):"
                     f <- getLine           
                     putStrLn ("TUITS DESDE "++f++":")
                     imprimeTuits (tuitsDesde (leeFecha f) ts) 
                     main
           "8" -> do putStrLn "\nEl trendingTopic es....."
                     print (trendingTopic ts)
                     main
           "9" -> return ()