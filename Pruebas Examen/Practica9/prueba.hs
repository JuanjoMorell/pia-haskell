--Para tener siempre activados patrones n+k
{-# LANGUAGE NPlusKPatterns #-}

--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}

type Mensaje = String
type Hashtag = String

borraRepetidos :: [a] -> [a]
borraRepetidos [] = []
borraRepetidos (h : hs)
    | elem (minusc h) hsM = borraRepetidos hs
    | otherwise = (minusc h) : borraRepetidos hs
    where hsM = minusc' hs
          minusc = map toLower
          minusc' = map minusc 

todosLosHashtags :: [Mensaje] -> [Hashtag]
todosLosHashtags = borraRepetidos.concat.(map buscaHashtag)
