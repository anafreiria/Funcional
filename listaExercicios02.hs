
import Data.Char
-- 1. Soma dos quadrados dos números de 1 a 100
f1 :: Int
f1 = sum [x ^ 2 | x <- [1..100]]

-- 2. Função replicate usando list comprehension
replicando :: Int -> a -> [a]
replicando n x = [x | _ <- [1..n]]

-- 3. Gera todas as tuplas pitagóricas (x, y, z) com x² + y² = z² até um limite dado
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 4. Números perfeitos até um dado limite (número igual à soma de seus divisores próprios)
somaDivisores :: Int -> Int
somaDivisores x = sum [d | d <- [1..(x `div` 2)], x `mod` d == 0]

perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [2..n], somaDivisores x == x]

-- 5. Reescrevendo uma list comprehension com concat
pares :: [(Int, Int)]
pares = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

-- 6. Função que encontra os índices de um elemento em uma lista
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where n = (length xs) - 1

find :: Eq a => a -> [(a, Int)] -> [Int]
find x pairs = [i | (y,i) <- pairs, x == y]

-- 7. Produto escalar entre duas listas
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = soma [ x*y | (x,y) <- zip xs ys]

soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

-- 8. Operador de potenciação (&!)
(&!) :: Int -> Int -> Int
_ &! 0 = 1
x &! y = x * (x &! (y - 1))

-- 9. Mapeando +7 nos números ímpares de 1 a 10
maisSeteImpares :: [Int]
maisSeteImpares = map (+7) (filter odd [1..10])

-- 10. Converte uma lista de dígitos em um número decimal
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

-- 11. Função unfold (gera lista com base em condição, transformação e sucessão)
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x       = []
  | otherwise = h x : unfold p h t (t x)

-- Exemplo: potências de 2 até ultrapassar 1024
potenciasDe2 :: [Int]
potenciasDe2 = unfold (> 1024) id (*2) 1

-- 12. Cubos dos números pares até um limite dado
par :: Int -> Bool
par n = n `mod` 2 == 0

evenCubes :: Int -> [Int]
evenCubes x = [i^3 | i <- [0..x], par i]

evenCubesMap :: Int -> [Int]
evenCubesMap x = map (^3) (filter par [0..x])

-- 13. Insere um número em uma lista ordenada, mantendo a ordem
inserOrd :: Int -> [Int] -> [Int]
inserOrd x xs = [y | y <- xs, y < x] ++ [x] ++ [y | y <- xs, y >= x]

-- 14. Conta quantos múltiplos de n existem no intervalo [a, b]
multiplos :: Int -> Int -> Int -> Int
multiplos n a b = tam [x | x <- [a..b], x `mod` n == 0]

tam :: [Int] -> Int
tam [] = 0
tam (x:xs) = 1 + tam xs

-- 15. Replica uma string n vezes, usando list comprehension e fold
replicante :: String -> Int -> String
replicante s n = concat [s | _ <- [1..n]]

replicateFold :: String -> Int -> String
replicateFold s n = foldr (++) "" [s | _ <- [1..n]]

-- 16. Adiciona '>' à esquerda de uma string até atingir o tamanho n
pushRight :: String -> Int -> String
pushRight s n
  | n <= length s = s
  | otherwise     = ['>' | _ <- [1..(n - length s)]] ++ s

-- 17. Inverte uma lista de inteiros
invertendo :: [Int] -> [Int]
invertendo xs = [xs !! i | i <- [length xs - 1, length xs - 2 .. 0]]

-- 18. Separa uma lista de inteiros em pares e ímpares (duas formas)
separarParesImpares :: [Int] -> ([Int], [Int])
separarParesImpares xs = ([x | x <- xs, even x], [x | x <- xs, odd x])

separarOutraForma :: [Int] -> ([Int], [Int])
separarOutraForma xs = (filter even xs, filter odd xs)

-- 19. Dada uma lista de inteiros, restorne uma String contendo as letras do alfabeto correspondentes

converte :: [Int] -> String
converte xs = [chr(n + 64)| n <- xs, n >= 1, n <= 26]

--20. Dada uma [Char], retorne a quantidade de ocorrências de um Char na String
iguais :: String -> Char -> Int
iguais s c = length [x | x <- s, x == c]

-- 21. dada uma lista de inteiros, ela retorne uma lista com uma repetição de cada elemento de acordo com seu valor
prolifera :: [Int] -> [Int]
prolifera xs = [x| x <- xs, x > 0, _ <- [1..x]]

-- 22. Dada uma lista de caracteres, retorne uma lista com cada caractere repetido de acordo com sua posição no alfabeto (A=1, B=2, ..., Z=26)
proliferaChar :: String -> String
proliferaChar xs = [x | x <- xs, _ <- [0..(ord x - ord 'A')]]


