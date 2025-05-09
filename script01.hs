{-Assunto: Litas
  Os conceitos introdutórios sobre listas foram apresentados em sala.
  Agora, considerando os casos mais simples, com apenas listas de inteiros,
  implemente as funções abaixo, considerando os operadores ++ e :
    ++ (concatena listas)
     : (insere um elemento na lista)
-}
     
{- 01 função que soma os elementos de uma lista -}
sumList::[Int]->Int
sumList [] = 0
sumList (a:bs) = a + sumList bs


{- 02-localiza elemento em lista -}
searchList::Int->[Int]->Bool
searchList _ [] = False
searchList a (b:bs)
    | a == b = True
    |otherwise = searchList a bs

{-03 remove todas ocorrências de y em uma lista -}
deleteList::Int->[Int]->[Int]
deleteList _ [] = []
deleteList y (x:xs)
    |y == x = deleteList y xs
    |otherwise = x : deleteList y xs


{-04 informa o tamanho de uma lista -}
lenghtList::[Int]->Int
lenghtList [] = 0
lenghtList (x:xs) = 1 + lenghtList xs


{-05 conta a ocorrência de um Int em [Int] -}
contList::Int->[Int]->Int
contList _ [] = 0
contList a (x:xs)
    |a==x = 1 + contList a xs
    |otherwise = contList a xs

{- 06 inverte a lista -}
reverseList:: [Int]->[Int]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

{- 07 inverte elementos das listas internas -}

reverseInternas :: [[Int]] -> [[Int]]
reverseInternas [] = []
reverseInternas (x:xs) = reverseList x : reverseInternas xs

{- 08 função que exclui a penúltima ocorrência de um número na lista-}
app :: [Int]->Int->Int->[Int]
app [] _ _ = [] 
app l _ 0 = l
app l _ 1 = l
app (a:b) x i 
  | a==x && i ==2 = b
  | a==x          = a:(app b x (i-1))
  | otherwise     = a:(app b x (i)) 

appS::[Int]->Int->[Int]
appS l x = app l x (ocorrencia l x)

-------------------------------------------------------------
{- Exercícios
     Implementar as funções: 
       myHead que recebe uma lista x e retorna a cabeça de x
       myTail que recebe uma lista x e retorna a lista x sem a cabeça
       myLast que recebe uma lista x e retorna o último elemento de x
       myInit que recebe uma lista x e retorna a lista x sem o último elemento
-}    

{-myHead que recebe uma lista x e retorna a cabeça de x-}

myHead :: [Int] -> Int
myHead [] = error "Lista vazia"
myHead (x:_) = x

{- myTail que recebe uma lista x e retorna a lista x sem a cabeça -}

myTail :: [Int] -> [Int]
myTail [] = error
myTail (x:xs) = xs

{- myLast que recebe uma lista x e retorna o último elemento de x -}

myLast :: [Int] -> Int
myLast [] = error "Lista vazia"
myLast [x] = x
myLast (_:xs) = myLast xs

{- myInit que recebe uma lista x e retorna a lista x sem o último elemento -}

myInit :: [Int] -> [Int]
myInit [] = error "Lista vazia"
myInit [x] = []
myInit (x:xs) = x : myInit xs

{- Exercício: 
   Implementar a função que retorna o maior elemento de uma lista -}

maiorValor :: Int -> Int -> Int
maiorValor m n
    | m >= n = m
    | otherwise = n

{- maiorElemento: recebe uma lista de inteiros e retorna o maior elemento -}|
maiorElemento :: [Int] -> Int
maiorElemento [x] = x
maiorElemento (x:xs) = maiorValor x (maiorElemento xs)







