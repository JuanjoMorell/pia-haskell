--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-}


reverse2lineas :: IO()
reverse2lineas = do {lin1 <- getLine;
                    lin2 <- getLine;
                    putStrLn(reverse lin2);
                    putStrLn(reverse lin1)}

copy :: IO()
copy = do linea <- getLine
          putStrLn linea
          copy

