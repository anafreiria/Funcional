import Data.List

fold :: (a -> a -> a) -> [a] -> a
fold _ []     = error "Lista vazia"
fold _ [a]    = a
fold op (a:bx) = op a (fold op bx)


-- Funções de alta ordem em Haskell:
-- São funções que recebem outras funções como argumentos ou retornam funções como resultado.

----------------------------------------------------
-- 1. map :: (a -> b) -> [a] -> [b]
-- Aplica uma função a cada elemento de uma lista, retornando uma nova lista com os resultados.

-- Exemplo:
-- map (+1) [1,2,3] == [2,3,4]

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

----------------------------------------------------
-- 2. filter :: (a -> Bool) -> [a] -> [a]
-- Seleciona os elementos da lista que satisfazem um predicado (função que retorna Bool).

-- Exemplo:
-- filter even [1,2,3,4] == [2,4]

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

----------------------------------------------------
-- 3. foldr :: (a -> b -> b) -> b -> [a] -> b
-- Reduz a lista da direita para a esquerda (right fold).

-- Exemplo:
-- foldr (+) 0 [1,2,3] == 1 + (2 + (3 + 0)) == 6

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc []     = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

----------------------------------------------------
-- 4. foldl :: (b -> a -> b) -> b -> [a] -> b
-- Reduz a lista da esquerda para a direita (left fold).

-- Exemplo:
-- foldl (+) 0 [1,2,3] == ((0 + 1) + 2) + 3 == 6

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs
