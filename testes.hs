import Data.Char


{- Assunto: listas e tuplas -}
periodo :: Int
periodo = 7

-- tabela de vendas
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 1198
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30


{-Testando funções script 01-}

-- 01 função que soma os elementos de uma lista

somaListinha :: [Int] -> Int
somaListinha [] = 0
somaListinha (a:xs) = a + somaListinha xs

-- 02-localiza elemento em lista
procuraListinha :: Int -> [Int] -> Bool
procuraListinha _ []= False
procuraListinha a (x:xs)
    | a == x = True
    | otherwise = procuraListinha a xs


-- 03 remove todas ocorrências de y em uma lista
removeListinha :: Int -> [Int] -> [Int]
removeListinha _ [] = []
removeListinha y (x:xs)
    | y == x = removeListinha y xs
    | otherwise = x : removeListinha y xs

-- 04 informa o tamanho de uma lista
tamanhoListinha :: [Int] -> Int
tamanhoListinha [] = 0
tamanhoListinha (a:ab) = 1 + tamanhoListinha ab

-- 05 conta a ocorrência de um Int em [Int]
contaOcorrencia :: Int -> [Int] -> Int
contaOcorrencia _ [] = 0
contaOcorrencias y (x:xs)
    | y == x = 1 + contaOcorrencia y xs
    |otherwise = contaOcorrencia y xs

-- 06 inverter uma lista de inteiros
inverteListinha :: [Int] -> [Int]
inverteListinha [] = []
inverteListinha (q:ws) = inverteListinha ws ++ [q]

-- 07 inverte elementos das listas internas

inverteInternas :: [[Int]] -> [[Int]]
inverteInternas [] = []
inverteInternas ((x:xs): ys) = inverteListinha (x:xs) : inverteInternas ys

-- 08 função que acha o maior valor entre dois elementos
maiorValor :: Int -> Int -> Int
maiorValor m n 
    | m >= n = m
    | otherwise = n

-- 09 função que acha o maior valor de uma lista
maiorValorListinha :: [Int] -> Int
maiorValorListinha [a] = a 
maiorValorListinha (a:ax) = maiorValor a (maiorValorListinha ax)

{-Testando funções script 02-}

-- 01 função retorna o total de vendas do período
totalVendas :: Int-> Int
totalVendas 0 = 0
totalVendas d = vendas (d) + totalVendas (d-1)

-- 02 encontra o dia em que mais se vendeu no período - versão 02 (tem como parâmetros o período e a maior venda)

diaMaiorVenda :: Int -> Int -> Int
diaMaiorVenda p dm 
    | p < 0 = -1
    | vendas p == dm = p
    | otherwise = diaMaiorVenda (p-1) dm


-- 03 encontra o maior valor entre dois inteiros -}
maxi :: Int -> Int -> Int
maxi m n
   |m >= n = m
   |otherwise = n

-- 04 encontra a maior venda, exercício: implemente essa função com apenas dois parâmetros e fazendo uso de maxi no código interno
maiorVenda :: Int -> Int -> Int
maiorVenda 0 y = y 
maiorVenda x y = maiorVenda (x-1) (maxi (vendas (x-1)) y)


maiorVenda2 :: Int -> Int -> Int
maiorVenda2 0 y = y
maiorVenda2 x y
    | vendas (x-1) > y = maiorVenda2 (x-1) (vendas (x-1))
    | otherwise = maiorVenda2 (x-1) y


-- 05 média das vendas
mediaVendas :: Int -> Float
mediaVendas d = fromIntegral (totalVendas d) / fromIntegral d

{-Testando funções script 03-}
-- 01 função que retorna uma lista de vendas
listaVendinhas :: Int -> [Int]
listaVendinhas (-1) = []
listaVendinhas n = vendas n : listaVendinhas(n-1)

-- 02 função que retorna [[Int]] com listas de dia e venda
listaDiaVendinhas :: Int -> [[Int]]
listaDiaVendinhas (-1) = []
listaDiaVendinhas d = [d, vendas d] : listaDiaVendinhas (d-1)

-- 03 função que ordena uma lista de inteiros
ordenaListinha :: [Int] -> [Int]
ordenaListinha [] = []
ordenaListinha (a:ax) = insere a (ordenaListinha ax)

insere :: Int -> [Int] -> [Int]
insere x [] = [x]
insere x (y:ys)
    | x <= y = x:y:ys
    | otherwise = y : insere x ys

-- 04 função que ordena [[Int]] pelo primeiro Int de cada lista 
ordenaListaDLista :: [[Int]] -> [[Int]]
ordenaListaDLista [] = []
ordenaListaDLista (a:ax) = insereDLista a (ordenaListaDLista ax)

insereDLista :: [Int] -> [[Int]] -> [[Int]]
insereDLista x [] = [x]
insereDLista x (y:ys)
    |head x <= head y = x:y:ys
    |otherwise = y : insereDLista x ys


--------------  tuplas ---------------------------------------
-- 06 função que gera uma lista de tuplas com dia e venda 
tuplaDiaVenda :: Int -> [(Int, Int)]
tuplaDiaVenda (-1) = []
tuplaDiaVenda x = (x, vendas x) : tuplaDiaVenda (x-1)

-- 07 função que gera o total de vendas
totalVendasTuplas :: [(Int, Int)] -> Int
totalVendasTuplas [] = 0
totalVendasTuplas ((_,x):bx) = x + totalVendasTuplas bx

--  08 função que retorna a maior venda

maiorVendaTupla :: Int -> [(Int, Int)] -> Int
maiorVendaTupla mv [] = mv
maiorVendaTupla mv ((a,b):xs) = maiorVendaTupla (maxi mv b) xs   

-- 08 passando só a lista de tuplas 
maiorVendaTupla2 :: [(Int,Int)] -> Int
maiorVendaTupla2 [] = 0
maiorVendaTupla2 ((a,b):ax) = maxi b (maiorVendaTupla2 ax)


-- 09 função que retorna os dias das maiores vendas
diasMaiorVendas :: [(Int, Int)] -> [Int]
diasMaiorVendas [] = []
diasMaiorVendas ((a,b):xs)
    | b == maiorVendaTupla2 ((a,b):xs) = a: diasMaiorVendas xs
    | otherwise = diasMaiorVendas xs


{- Testando funções do script 04 -}

-- 01 função que separa [(Int,Char)] em ([Int],[Char])
separaTuplinhas :: [(Int, Char)] -> ([Int], [Char])
separaTuplinhas x = (soIntinho x, soCharzinho x)

soIntinho :: [(Int, Char)] -> [Int]
soIntinho [] = []
soIntinho ((i,_):xs) = i : soIntinho xs

soCharzinho :: [(Int, Char)] -> [Char]
soCharzinho [] = []
soCharzinho ((_,c):xs) = c : soCharzinho xs


-- 02 versão em uma única função
separaTuplinhas2 :: [(Int, Char)] -> ([Int], [Char])
separaTuplinhas2 [] = ([], [])
separaTuplinhas2 ((i,c):xs) = (i:is, c:cs)
    where (is, cs) = separaTuplinhas2 xs

-- 03 função que junta duas listas em lista de duplas
juntaListas :: [Bool] -> [Char] -> [(Bool, Char)]
juntaListas [] _ = []
juntaListas _ [] = []
juntaListas (b:bs) (c:cs) = (b,c) : juntaListas bs cs

-- 04 função que recebe [Char] e retorna [(Bool,Char)]
-- True se Char for alfanumérico e False, caso contrário
setAlfa :: String -> [(Bool, Char)]
setAlfa [] = []
setAlfa (c:cs) = (isAlphaNume c, c): setAlfa cs

isAlphaNume :: Char -> Bool
isAlphaNume c
    | c >= '0' && c <= '9' = True
    | c >= 'A' && c <= 'Z' = True
    | c >= 'a' && c <= 'z' = True
    | otherwise = False


-- 05 função que recebe [(Bool, Char)] e filtra alfanuméricos
filtraAlfinhos :: [(Bool, Char)] -> String
filtraAlfinhos [] = []
filtraAlfinhos ((b,c):xs)
    | b = c : filtraAlfinhos xs
    | otherwise = filtraAlfinhos xs

-- 06 função transforma String de alfa em [Int]
alfaParaInt :: String -> [Int]
alfaParaInt [] = []
alfaParaInt (a:ax) = ord a : alfaParaInt ax

-- 07 função que gera tabela ascii
geraTabelinha :: Int -> [(Int, Char)]
geraTabelinha x = [(x, chr x) | x <- [0..x]]

{- Testando funções do scripy 05 -}

{- 01. faça a função f1 que receba uma String S e Retorne uma String R.
R deve ser igual a S, com exceção de que todo caracter alfanumérico x encontrado em S
que esteja seguido de um caracter y não alfanumérico determinará que y será repetido x vezes em R

exemplo  f1 "ab42c570sd3f" retorna "ab42cc570d3fff" 
-}

f1Minha :: String -> String
f1Minha [] = []
f1Minha [a] = [a]
f1Minha (a:b:xs)
    | isDigit a && not(isDigit b) = a : (replica (num a) b) ++ f1Minha xs
    | otherwise = a : f1Minha xs

num :: Char -> Int
num a = ord (a) - ord ('0')

replica :: Int -> Char -> String
replica 0 _ = []
replica i c = c : replica (i-1) c
    

{- 02. faça f11, outra versão de f1, que retorne R do tipo [(Char, Bool, Int)] 
de modo que, para cada caractere de S, informe se ele será repetido ou não
e a quantidade de vezes. -}

f11Minha :: String -> [(Char, Bool, Int)]
f11Minha [] = []
f11Minha [a] = [(a, False, 1)]
f11Minha (a:b:xs)
    | isDigit a && not(isDigit b) = (a,False,1) : (b, True, num a) : f11Minha xs
    | otherwise = (a, False, 1) : f11Minha (b:xs)


-- 03. faça f111 que receba [(Char, Bool, Int)] e gere uma String com os caracteres repetidos ou não (como R em f1). Use o Bool da dupla -}

f111Minha :: [(Char, Bool, Int)] -> String
f111Minhha [] = []
f111Minha ((c,b,i):xs)
    | b = replica i c ++ f111Minha xs
    | otherwise = c: f111Minha xs


-- 04. faça a função f2 que receba uma lista de Strings e aplique a todas as strings a commputação da função f1

f2Minha :: [String] -> [String]
f2Minha [] = []
f2Minha [x] = [f1Minha x]
f2Minha (x:xs) = f1Minha x : f2Minha xs

{-Testando funções script 06-}

-- 01 função que retorna lista de duplas com char e posição na ASCII
listaAscii :: Int -> [(Char, Int)]
listaAscii n = suporte 97 n

suporte :: Int -> Int -> [(Char, Int)]
suporte _ 0 = []
suporte x n = (chr x, x) : suporte (x+1) (n-1)

-- 02 função meuChar que pesquisa um char pelo int na lista gerada
meuCharzinho :: Int -> [(Char, Int)] -> Char
meuCharzinho _ [] = error "Valor não encontrado na lista"
meuCharzinho n ((c, i):xs)
    | n == i = c
    | otherwise = meuCharzinho n xs

-- 03 função meuOrd que pesquisa o int pelo char na lista gerada
meuOrdzinho :: Char -> [(Char, Int)] -> Int
meuOrdzinho _ [] = error "Valor não encontrado na lista"
meuOrdzinho c ((x, i):xs)
    | c == x = i
    | otherwise = meuOrdzinho c xs


-- {-05 seja o tipo [(Bool, [Int])]. Faça uma função que ordena [Int] quando o booleano é True. Também, passe o Bool para False, quando ordenar [Int]

ordenaListaDuplinha :: [(Bool, [Int])] -> [(Bool, [Int])]
ordenaListaDuplinha [] = []
ordenaListaDuplinha ((b, i):xs)
    | b = (False, ordenaListinha i) : ordenaListaDuplinha xs
    | otherwise = (b,i) : ordenaListaDuplinha xs


{-Algumas funções usando chr e ord-}
-- 01 Implemente uma função nextChar :: Char -> Char que recebe um caractere e retorna o próximo caractere na tabela Unicode.

nextChar :: Char -> Char
nextChar x = chr(ord x + 1)

--02. Implemente uma função shiftChar :: Int -> Char -> Char que recebe um número inteiro n e um caractere, e retorna o caractere que está n posições à frente na tabela Unicode.
shiftChar :: Int -> Char -> Char
shiftChar n x = chr(ord x + n)

--03. Faça uma função charDistance :: Char -> Char -> Int que recebe dois caracteres e retorna a distância entre eles na tabela Unicode.

charDistancia :: Char -> Char -> Int
charDistancia x n = (ord x - ord n)

--04. Implemente uma função toUpperCase :: Char -> Char que transforma uma letra minúscula em maiúscula usando apenas chr e ord (sem usar funções prontas como toUpper).

toUperMinha :: Char -> Char
toUperMinha x
    | x >= 'a' && x <='z'= chr (ord x - 32)
    | otherwise = x

--05. Crie uma função cipher :: Int -> String -> String que aplica uma cifra simples em uma string, deslocando cada caractere n posições para frente (estilo cifra de César).

cipher :: Int -> String -> String
cipher n texto = [shiftChar n c | c <- texto]

-- Implemente a função alphaToInt :: Char -> Int que recebe uma letra minúscula entre 'a' e 'z' e retorna sua posição no alfabeto (começando em 0).
alphaToInt :: Char -> Int
alphaToInt c
    | c >= 'a' && c <= 'z' = ord c - ord 'a'
    | otherwise = error "Caractere fora do intervalo permitido"