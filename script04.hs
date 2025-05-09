{- objetivo: trabalhar tipos distintos entre listas e tuplas -}

import Data.Char


-- ------------------------------------------------------
-- {- 01 função que separa [(Int,Char)] em ([Int],[Char]) -}


separaTuplas :: [(Int, Char)] -> ([Int], [Char])
separaTuplas xs = (sóInts xs, sóChars xs)

sóInts :: [(Int, Char)] -> [Int]
sóInts [] = []
sóInts ((n, _) : xs) = n : sóInts xs

sóChars :: [(Int, Char)] -> [Char]
sóChars [] = []
sóChars ((_, c) : xs) = c : sóChars xs


-- {- 02 versão em uma única função -}
-- --myUnzipU :: [(Int,Char)]->([Int],[Char])
myUnzip :: [(Int, Char)] -> ([Int], [Char])
myUnzip [] = ([], [])
myUnzip ((i, c):xs) = (i:is, c:cs)
  where (is, cs) = myUnzip xs

-- ------------------------------------------------------------
-- {- 03 função que junta duas listas em lista de duplas -}
-- --myZip::[Bool]->[Char] ->[(Bool,Char)]  

myZip :: [Bool] -> [Char] -> [(Bool, Char)]
myZip [] _ = []
myZip _ [] = []
myZip (b:bs) (c:cs) = (b, c) : myZip bs cs


{- 04 função que recebe [Char] e retorna [(Bool,Char)] 
 True se Char for alfanumérico e False, caso contrário -}
--setAlfa::String -> [(Bool,Char)] 

setAlfa :: String -> [(Bool, Char)]
setAlfa [] = []
setAlfa (c:cs) = (isAlphaNum c, c) : setAlfa cs

   
-- {- 05 função que recebe [(Bool, Char)] e filtra alfanuméricos -}
-- --filtraAlfa:: [(Bool,Char)] -> String

filtraAlfa :: [(Bool, Char)] -> String
filtraAlfa [] = []
filtraAlfa ((b, c):xs)
  | b         = c : filtraAlfa xs  -- Se 'b' for True, inclui 'c'
  | otherwise = filtraAlfa xs        -- Se 'b' for False, ignora 'c'


-- {- 06 função transforma String de alfa em Int -}
-- --alfaToInt::String -> [Int]
alfaToInt :: String -> [Int]
alfaToInt = map ord

-- {-O map é usado para aplicar a função ord (que converte um Char em Int)
-- a cada caractere da String fornecida, gerando a lista de códigos ASCII 
-- correspondente. Se quiser entender melhor como o map funciona, ele
--  aplica uma função em todos os elementos de uma lista e retorna uma nova lista com os resultados. Neste caso, a função é ord.-}

-- {-- 07 função que gera tabela ascii -}
-- --geraASCII::Int->[(Int,Char)]

geraASCII :: Int -> [(Int, Char)]
geraASCII n = [(i, chr i) | i <- [0..n]]



