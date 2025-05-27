{-1. Utilizando list comprehension, gere uma expressão que calcule 1^2 + 2^2 + ...100^2 .-}
f1 :: Int
f1 = sum [x ^ 2 | x <- [1,2..100]]

{-2. De maneira similar à função length, mostre como a função replicate :: Int -> a -> [a], que
retorna uma lista de elementos idênticos, pode ser definida utilizando list comprehension.
Main> replicate 3 True = [True, True, True]-}

replicando :: Int -> a -> [a]
replicando n x = [x| _ <- [1..n]]