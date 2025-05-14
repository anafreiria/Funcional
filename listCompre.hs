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

-- Ideia vista baseada em livro, deve ser testada (não sei se funciona)
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