{-# LANGUAGE NoMonomorphismRestriction #-}
-- Lista 5: Marcos Paulo Ferreira
-- (um pouco atrasado, por favor considerar :D)

import Control.Applicative ((<*>), liftA3)
import Data.Maybe (fromMaybe)
import Data.Map (Map, findWithDefault, empty, insert)

--1) Questao 1
data Id a = Id a deriving Show
data Tres a b c = Tres a b c deriving Show

instance Functor Id where
  fmap f (Id a) = Id (f a)

instance Functor (Tres a b) where
  fmap f (Tres a b c) = Tres a b (f c)

--2) Questao 2
id_funtor :: (Functor f, Eq (f a)) => f a -> Bool
id_funtor x = fmap id x == x
comp_funtor::(Functor f, Eq(f c))=>(a->b)->(b-> c)->f a-> Bool
comp_funtor f g x = fmap g (fmap f x) == fmap (g . f) x

-- nao consegui usar as "funcoes" acima. Exemplo:
-- id_funtor (Id 5)
--
-- isso retorna erro. Entao executei o corpo das funcoes.

{-
  -- Id type
  fmap id (Id a)
  Id (id a)
  Id a

  fmap (f . g) (Id a)
  Id ((f . g) a)
  Id (f(g a))
  fmap f (Id (g a))
  fmap f (fmap g (Id a))

  -- Tres type
  fmap id (Tres a b c)
  Tres a b (id c)
  Tres a b c
  
  fmap (f . g) (Tres a b c)
  Tres a b ((f . g) c)
  Tres a b (f(g c))
  fmap f (Tres a b (g c))
  fmap f (fmap g (Tres a b c))
-}

--3) Questao 3
data AouB = A | B
---- NÂO. não possui elementos internos (ex: A a ou B b)

data F f a = F (f a) deriving Show
instance (Functor f) => Functor (F f) where
  fmap g (F a) = F (fmap g a)
--Exemplo: fmap (+3) (F (Just 5))

data G a f = G (f a)
-- Não é possível.

data X c d = C c | D d deriving Show
instance Functor (X c) where
  fmap f (C c) = C c
  fmap f (D d) = D (f d)

---- 5.2.2
--1) Questao 1

-- Par
data Pair a = Pair a a deriving Show
instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')
instance Applicative Pair where
  pure a = Pair a a
  (Pair f f') <*> (Pair a a') = Pair (f a) (f' a')
  
-- Two
data Two a b = Two a b deriving Show
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
instance Applicative (Two a) where
  pure b = Two undefined b
  (Two f f') <*> (Two a b) = Two a (f' b)

--2) Questao 2
sequenceAL:: Applicative f => [f a] -> f [a]
sequenceAL [] = pure []
sequenceAL (fa:fx) = (:) <$> fa <*> sequenceAL fx

readInt :: IO Int
readInt = getLine >>= \s -> return (read s :: Int)

read_sum :: IO Int
read_sum = pure (+) <*> readInt <*> readInt

collect_n :: Int -> [IO Int]
collect_n 1 = [readInt]
collect_n n = (:) readInt $ collect_n (n - 1)

sum_n_from_input :: Int -> IO Int
sum_n_from_input n = pure sum <*> (sequenceAL $ collect_n n)

--3) Questao 3
-----------------------------------------
-- Usando Map
-----------------------------------------

type Env = Map String Int
data Exp = Var String | Val Int | Add Exp Exp

eval:: Exp -> Env -> Int
eval (Var x) e = fetch x e
eval (Val i) _ = i
eval (Add p q) e = eval p e + eval q e
-----------------------------------------
eval':: Exp -> Env -> Int
eval' (Var x) = fetch x
eval' (Val i) = const i
eval' (Add p q) = const (+) <*> eval' p <*> eval' q

fetch:: String -> Env -> Int
fetch v = findWithDefault
  (error ("undef. var. " ++ show v ++ "\n")) v

ex_eval1 = eval (Add (Val 1) (Val 2)) empty
ex_eval1' = eval' (Add (Val 1) (Val 2)) empty
ex_eval2 = eval (Add (Val 1) (Var "x")) mx
ex_eval2' = eval' (Add (Val 1) (Var "x")) mx
mx = insert "x" 2 empty

-----------------------------------------
-- Usando Lista
-----------------------------------------
type EnvLis = [(String, Int)]
evalLis::Exp -> EnvLis -> Int
evalLis (Var x) = fetchLis x
evalLis (Val i) = const i
evalLis (Add p q) = const (+) <*> evalLis p <*> evalLis q

fetchLis :: String -> EnvLis -> Int
fetchLis v e = case lookup v e of
  Just k -> k
  Nothing -> (error ("undef. var. " ++ show v ++ "\n"))

mx' = ("x", 2) : []

ex_eval1'' = evalLis (Add (Val 1) (Val 2)) []
ex_eval2'' = evalLis (Add (Val 1) (Var "x")) mx'

