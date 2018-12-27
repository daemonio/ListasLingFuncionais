{-# LANGUAGE NoMonomorphismRestriction #-}

-- Lista 6: Marcos Paulo Ferreira

import Control.Monad
import Control.Monad.State

-- 1) Questao 1
data Pessoa = Pessoa {nome::Nome, idade::Idade, id::RG}
type Nome = String
type Idade = Integer
type RG = String

somaidade :: [Pessoa] -> Integer
somaidade xs = foldl (\x y  -> (idade y) + x) 0 xs

-- 2) Questao 2
aproxime n = foldl (\x y -> x + (1 / (fat y))) 0 [1..n]
              where fat n = foldl (*) 1 [1..n]

-- 3) Questao 3
{-
    f x y z = x z (y z)
    tx -> ty -> tz -> tr

    ty = tz -> t1
    tx = tz -> t2
    t2 = t1 -> tr

    tx = tz -> t1 -> tr

    Fazendo:

    tz = a, t1 = b, tr = c

    (a -> b -> c) -> (a -> b) -> c
-}

-- 4) Questao 4
-- a)
for 1 m = m >> return ()
for n m = m >>  for (n-1) m

-- b)
-- while :: m Bool -> m a -> m ()
while b c = do {v <- b; if v then c >> while b c else return() }

-- Para testar o while:
-- let ler = do {v <- getChar; if v /= '5' then ler else return(True)}
-- let escrever = do { putStrLn "Numero Cinco" }
-- while ler escrever

-- 4) Questao 4
type Valor = Int
type Estado = (Bool , Valor )
valor (_ , v ) = v
joga :: String -> State Estado Valor
joga (x : xs) = do
    (ligado , v) <- get
    case x of
        '+' | ligado -> put ( ligado , v + 1)
        '-' | ligado -> put ( ligado , v - 1)
        'l'          -> put ( ligado , v )
        'd'          -> put ( not ligado , v )
        _            -> put ( ligado , v )
    joga xs
joga [] = get >>= return . valor

estadoInicial = (False , 0)
main = print $ evalState ( joga "+-l+++d--l+--+-" ) estadoInicial
