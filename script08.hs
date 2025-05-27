-- This is a simple list comprehension example


-- Função que gera uma lista de inteiros multiplicando cada elemento da lista l pelo inteiro x
f1 :: Int -> [Int] -> [Int]
f1 x l = [x * a | a <- l]

-- Função que multiplica os elementos da lista l por x, mas apenas se o elemento for par
f2 :: Int -> [Int] -> [Int]
f2 x l = [a * x | a <- l, (mod) a 2 == 0]

-- Funça que multiplica os pares por x, mas mantém os ímpares na lista

f3_normal :: Int -> [Int] -> [Int]
f3_normal x [] = []
f3_normal x (a:b)
    | (mod) a 2 == 0 = (x * a) : f3_normal x b
    | otherwise = a : f3_normal x b

f3 :: Int -> [Int] -> [Int]
f3 x l = [x*a | a <- l, (mod) a 2 == 0] ++ [a | a <- l, (mod) a 2 /= 0]

-- Ideia vista em livro, deve ser testada
f3_teste :: Int -> [Int] -> [Int]
f3_teste x l = [if (mod) a 2 == 0 then (x * a) else a | a <- l] 


-- lista com elementos * x pares ou maiores que 5
f4 :: Int -> [Int] -> [Int]
f4 x l = [a * x | a <- l, (mod) a 2 == 0 || a > 5]

-- filtram pares maiores que 5 e multiplicam por x
f5 :: Int -> [Int] -> [Int]
f5 x l = [a * x | a <- l, (mod) a 2 == 0 && a > 5]

-- lista com elementos * x pares ou menores que 5
f6 :: Int -> [Int] -> [Int]
f6 x l = [a * x | a <- l, (mod) a 2 == 0 || a < 5]



-- uso do $$
--------------------------------
--zip [1,2,3] ["a","bb", "ccc"]
--unzip $$
--------------------------------
-- 3+5
-- 32 - $$

{- função que multiplica por x cada elemento de uma lista -}
f1_e x l = [a*x| a<-l]

{- função que filtra os pares e os multiplica por x-}
f2_e x l = [a*x| a<-l, (mod) a 2 == 0]

{- função que multiplica por x apenas os números pares de uma lista -}
f3_e x l = [a*x| a<-l, (mod) a 2 == 0] ++ [a| a<-l, (mod) a 2 /= 0]  

{- função que filtra os pares maiores que 5 e os multiplica por x- -}
f4_e x l = [a*x| a<-l, (mod) a 2 == 0, a > 5]  

{- função que filtra os pares ou maiores que 5 e os multiplica por x- -}
f5_e x l = [a*x| a<-l, (mod) a 2 == 0 || a > 5]  

{- função que faz um produto cartesiano entre duas listas
   sendo uma dada como parâmetro e outra gerada dentro da função-}

produtoCartesiano :: [a] -> [(a, Int)]
produtoCartesiano xs = [(x, y) | x <- xs, y <- [1..4]]











