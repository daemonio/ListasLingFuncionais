{-# LANGUAGE NoMonomorphismRestriction #-}

-- Lista 2: Marcos Paulo Ferreira
-- (um pouco atrasado, por favor considerar :D)

import Data.Char

--1) Questao 1

concat3 :: [String] -> String
concat3 = concat . foldr (\x y -> take 3 x : y) []

--2) Questao 2

data Pessoa = Pessoa {nome::Nome, idade::Idade, id::RG} deriving Show
type Nome = String
type Idade = Integer
type RG = String

soma_idade :: [Pessoa] -> Integer
soma_idade = foldr (\x y -> (idade x) + y) 0

-- Nota: idade x retorna o campo Idade do elemento.

--3) Questao 3

pessoa_mais_nova :: [Pessoa] -> Nome
pessoa_mais_nova (x:xs) = nome $ foldr compara_idade_pessoa x xs
    where compara_idade_pessoa p1 p2 = if (idade p1) < (idade p2) then p1 else p2

-- Nota: foldr retornará a pessoa então é necessário "nome" para retornar somente
-- o valor do campo nome.

--4) Questao 4

-- Exercicio feito em sala de aula.

ex4 = foldr ((++) . f) []
f x = (map toUpper . filter isLetter $ x1) ++ x2
 where (x1, x2) = splitAt 3 x

-- f chama split para criar listas com 3 ou menos particoes da lista original
-- x1 é os 3 primeiros e x2 o restante
-- filter retorna aquilo que é letra no inicio de x1 e repassa para o toUpper
   -- filter ignora aquilo que não é letra dentro da lista de tamanho 3.
-- o resultado do map é concatenado com o restante, x2, fazendo ++ x2
-- Como f atua somente em uma "string" então usa-se um foldr por fora para aplicar
   -- em todas as strings da lista

--5) Questao 5

{-

Devo evaluação lazy, expressão só é avaliada quando necessário, que é o caso
do foldr se a função f de parâmetro for não estrita. Assim a execução da
função pode parar quando possível.

Usando foldl, a função é sempre avaliada.
-}

--6) Questao 6

remdups :: (Eq a) => [a] -> [a]
remdups [] = []
remdups (x:xs) = reverse $ foldl (\ys y -> if head ys == y
                                           then ys
                                           else y:ys
                                 ) [x] xs

-- Nota: foldl pega uma lista ys e um elemento y, aí comparado o head de ys com
-- o elemento y. Se igual, retorna a lista pois y ja esta incluso, se diferente
-- retorna a lista com y incluso, y:ys.
-- O elemento inicial do foldr é o primeiro elemento da lista, por isso o (x:xs)
-- é necessário.
-- O reverse é porque o foldl retornará a lista invertida.
