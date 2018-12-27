{-# LANGUAGE NoMonomorphismRestriction #-}

-- Lista 3: Marcos Paulo Ferreira
-- (um pouco atrasado, por favor considerar :D)

--1) Questao 1

{-

\x \y \z. x z (y z)

Como visto em sala:

f :: tx -> ty -> tz -> tr

ty = tz -> t1
tx = tz -> t1 -> tr

(tz -> t1 -> tr) -> (tz -> t1) -> tz -> tr
o
u
(a -> b -> c) -> (a -> b) -> a -> c

-}

--2) Questao 2

{-

tf -> tg -> Either tx t1 -> tr1
tf -> tg -> Either t2 ty -> tr2

mas tr1 = tr2 = tr

tf :: tx -> tr
tg :: ty -> tr

(tx -> tr) -> (ty -> tr) -> Either tx t1 -> tr
(tx -> tr) -> (ty -> tr) -> Either t2 ty -> tr

logo: tx = t2 e ty = t1. Fazendo: tx = a, ty =b, tr = c:

(a -> c) -> (b -> c) -> Either a b -> c

-}

--3) Questao 3

-- a)
-- String com menor primeiro (sala de aula)
exA :: (Ord a, Show a) => a -> a -> String
exA x y = if x < y then show x ++ " " ++ show y else show y ++ " " ++ show x

-- b)
exB :: (a -> b -> c) -> b -> a -> c
exB f y x = f x y

-- c)
exC :: (a -> b, a -> c) -> a -> (b, c)
exC (f,g) x = (f x, g x)

-- d)
-- Como c não é usado, tem que colocar um undefined no lugar.
exD :: (a -> b, b -> d) -> (a, b) -> (c, d)
exD (f, g) (x, y) = (undefined, g $ f x)

-- e)
-- composicao de funcoes com argumentos invertidos
exE :: (b -> c) -> (a -> b) -> a -> c
exE f g x = f $ g x

-- f)
-- é o mesmo que um unzip
exF :: [(a, b)] -> ([a], [b])
exF xs = (map fst xs, map snd xs)
  
-- g)
-- igual map mas função ja tem um parametro
exG :: (a -> b -> a) -> a -> [b] -> [a]
exG f a xs = map (f a) xs
