{-# LANGUAGE NoMonomorphismRestriction #-}

-- Lista 4: Marcos Paulo Ferreira
-- (um pouco atrasado, por favor considerar :D)

--1) Questao 1

import System.IO
import Data.Char

echo :: Int -> IO()
echo 0 = return ()
echo n = getLine >>= putStrLn >> echo (n-1)

echo_do :: Int -> IO()
echo_do 0 = return()
echo_do n = do {linha <- getLine; putStrLn linha; echo (n-1)}

--2) Questao 2
getAndApp :: Int -> (String -> String) -> IO ()
getAndApp 0 _ = return ()
getAndApp n f = do {linha <- getLine; putStrLn $ f linha; getAndApp (n-1) f}

-- sem do
--getAndApp n f = getLine >>= putStrLn . f >> getAndApp (n-1) f

--3) Questao 3
countL :: IO()
countL = f 0 where f n = do  linha <- getLine;
                              if null linha then putStrLn $ show n
                              else f (n+1)

--4) Questao 4
-- (como visto em sala de aula)
wc :: IO()
wc = do text <- getContents
        putStr $ nLines text
        putStr $ " " ++ nWords text
        putStr $ " " ++ nChar  text
        putStrLn ""
        where nLines text' = show $ length $ filter (== '\n') text'
              nWords text' = show $ length $ words text'
              nChar  text' = show $ length text'

--5) Questao 5
readInt :: IO Int
readInt = getLine >>= \s -> return (read s :: Int)

readintList :: Int -> [Int] -> IO()
readintList 1 xs = putStrLn $ show (max' xs) ++ " " ++ show (avg xs)
                   where avg xs = div (sum xs) (length xs)
                         max' (x:xs) = foldr max x xs

readintList 0 xs = do i <- readInt
                      if i <= 0 then readintList 1 xs else readintList 0 (i:xs)

readint_main :: IO()
readint_main = readintList 0 []
