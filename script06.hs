import Data.Char 

{-01 função que retorna lista de duplas com char e posição na ASCII -}
listaDuplaCharInt:: Int-> [(Char,Int)]
listaDuplaCharInt n = listaASCII 97 n

listaASCII:: Int-> Int-> [(Char,Int)]
listaASCII _ 0 = []
listaASCII x n = (chr x, x):listaASCII (x+1) (n-1)



{-02 função meuChar que pesquisa um char pelo int na lista gerada -}
meuChar :: Int -> [(Char, Int)] -> Char
meuChar _ [] = error "Valor não encontrado na lista"
meuChar n ((c, i):xs)
    | n == i    = c
    | otherwise = meuChar n xs


{-03 função meuOrd que pesquisa o int pelo char na lista gerada -}
meuOrd :: Char -> [(Char, Int)] -> Int
meuOrd _ [] = error "Valor não encontrado na lista"
meuOrd c ((x, i):xs)
    | c == x    = i
    | otherwise = meuOrd c xs


{-04 função que ordena uma lista de inteiros -}
ordenaLista::[Int]->[Int]
ordenaLista [] = []
ordenaLista (x:xs) = insere x (ordenaLista xs)

-- função auxiliar para inserir elemento na posição correta
insere :: Int -> [Int] -> [Int]
insere x [] = [x]
insere x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : insere x ys

{-05 seja o tipo [(Bool, [Int])]. 
Faça uma função que ordena [Int] quando o booleano é True. 
Também, passe o Bool para False, quando ordenar [Int]
exemplo: ordenaListaDupla [(True,[3,4,1,0,9]),(False,[]),(True,[4,3,2,1,0])]
retorna:                  [(False,[0,1,3,4,9]),(False,[]),(False,[0,1,2,3,4])]
-}

ordenaListaDupla::[(Bool, [Int])]->[(Bool, [Int])]
ordenaListaDupla [] = []
ordenaListaDupla ((b, l):xs)
    | b == True = (False, ordenaLista l) : ordenaListaDupla xs
    | otherwise  = (b, l) : ordenaListaDupla xs


exemplo :: [(Bool, [Int])]
exemplo = [(True, [3,1,2]), (False, [5,4]), (True, [9,7,8])]

{-Inverter uma lista-}

inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

{-Inverter uma lista de duplas-}
inverteDupla :: [(a, b)] -> [(b, a)]
inverteDupla [] = []
inverteDupla ((x, y):xs) = (y,x) : inverteDupla xs

{-Inverter uma lista de triplas-}
inverteTripla :: [(a, b, c)] -> [(c, b, a)]
inverteTripla [] = []
inverteTripla ((x, y, z):xs) = (z,y,x) : inverteTripla xs


