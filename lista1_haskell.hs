{-
  Lista 1 de Exercícios – Programação Funcional
  Aluno: Marcos Paulo Ferreira
-}

-- Questao 2.D)

-- Para Beaver seriam feitas n avaliações enquanto para Suzy somente uma avaliação.
-- Um melhor resultado para Beaver seria nem chamar o map, executando somente: f . head
-- Uma alternativa para: first p = head . filter p
-- Seria:

-- VAI DAR ERRO AQUI POIS F NAO EH DEFINIDO.
first :: (a -> Bool) -> [a] -> a
first p xs | null xs = error "Empty list"
 | p x = x
 | otherwise = first f p $ tail xs
 where x = head xs

-- Uma alternativa para: first p f = head . filter p . map f
-- Seria:

first2 :: (b -> Bool) -> (a -> b) -> [a] -> b
first2 p f xs | null xs = error "Empty list"
 | p x = x
 | otherwise = first2 p f $ tail xs
 where x = f (head xs)

-- Questao 2.F)

-- Tomaria n-1 multiplicações, que seria a quantidade de interações até se chegar em um
-- n igual ao caso base.
-- Um modo de conseguir menos que n-1 multiplicações seria:

exp' :: Integer -> Integer -> Integer
exp' x n
 | n == 0 = 1
 | n == 1 = x
 | odd n = x * y * y
 | even n = y * y
 where y = exp' x (n `div` 2)

-- Questao 2.H)

-- Transforma char para int
getDigit :: Char -> Int
getDigit c = read [c]

-- Recebe string e concatena com checksum
addSum :: String -> String
addSum cin = cin ++ show (n `div` 10) ++ show (n `mod` 10)
 where n = sum $ map getDigit cin

-- Valida um checksum
valid :: String -> Bool
valid cin = cin == addSum (take 8 cin)

-- Questao 3.F)

sqrt' :: Double -> Double -> Double
sqrt' x guess
  | guessIsGoodEnough = guess
  | otherwise = sqrt' x improvedGuess
  where
  -- Testa a precisao do guess
  precision = 0.00001
  guessIsGoodEnough = abs (guess * guess - x) < precision * x
  -- Melhora o nosso guess
  improvedGuess = (guess + x / guess) / 2

-- Uma melhor tentativa (impovedGuess) é obter a média (guess + x / guess) / 2 e
-- o erro relativo (precision * x) funciona melhor.

-- Questao 4.A)

{-
Quais são verdadeiras?
[]:xs = xs
[]:xs = [[],xs]
xs:[] = xs
xs:[] = [xs]
xs:xs = [xs,xs]
[[]] ++ xs = xs
[[]] ++ xs = [[],xs]
[[]] ++ [xs] = [[],xs]
[xs] ++ [] = [xs]


Testando no ambiente GHCI as alternativas verdadeiras são:

xs:[] = [xs]
[[]] ++ [xs] = [[],xs]
[xs] ++ [] = [xs]

Considerando o null como (== []), sua definição seria:

null :: Eq a => [a] -> Bool

Porém essa definição é bastante restritiva e não funciona para todo tipo de lista
(por exemplo, não funciona para lista de funções)
-}

-- Questao 4.C)

disjoint :: Ord a => [a] -> [a] -> Bool
disjoit ax@(a:x) by@(b:y)
 | a < b = disjoint x by
 | a > b = disjoint ax y
 | otherwise = False
disjoint _ _ = True

-- Questao 4.F)

data List a = Nil | Snoc (List a) a

head' :: List a -> a
head' (Snoc Nil x) = x
head' (Snoc xs x) = head' xs

last :: List a -> a
last (Snoc xs x) = x

toList :: [a] -> List a
toList = convert . reverse
 where convert [] = Nil
       convert (x:xs) = Snoc (convert xs) x

fromList :: List a -> [a]
fromList = reverse . convert
 where convert Nil = []
       convert (Snoc xs x) = x:convert xs

-- Questao 4.I)

{-
map (f . g) xs = map f (map g xs)

Como o Haskell utiliza a avaliação preguiçosa então a ordem da avaliação da composição de funções não importa,
como também não importa se a lista é finita ou infinita.

Porém o segundo lado da igualdade realiza um map desnecessário e gera uma lista intermediária para ser usada no
segundo map, enquanto o primeiro lado da igualdade não gera lista intermediára, o que otimiza a execução.

Com base nisso, as alternativas corretas são: 3, 4, 5 e 7.
-}
