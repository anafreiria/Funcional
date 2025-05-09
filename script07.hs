{-Objetivos: introduzir os conceitos de
   a) List Comprehension
   b) Função de alta ordem.
   
   Para tanto, iniciamos com um problema simples e mostramos,
   a cada passo, as possibilidades de melhorias.
   -}
import Data.Char

{- faça f1 capaz de somar uma lista de inteiros se um Char for alfanumérico, 
    ou multiplicar os elementos, caso contrário -}
f1::Char->[Int]->Int
f1 c x
    |isDigit c && x == [] = 0
    |not(isDigit c) && x == [] = 1
    |isDigit c = a + f1 c b
    |otherwise = a * f1 c b
        where (a:b) = x

{- reescreva f1 usando casamento de padrão -}
f2::Char->[Int]->Int
f2 c x = f2_aux (isDigit c) x

f2_aux :: Bool->[Int]->Int
f2_aux True [] = 0
f2_aux True (a:b) = a + f2_aux True b
f2_aux False [] = 1
f2_aux False (a:b) = a * f2_aux False b

{- reescreva f2 fazendo chamadas de funções para somar ou multiplicar -}
f3::Char->[Int]-> Int
f3 c x 
    |isDigit c = soma x
    |otherwise = mult x
        where
            soma [] = 0
            soma (a:b) = a + soma b
            mult [] = 1
            mult (a:b) = a * mult b

-- {- reescreva f3 usando função de alta ordem
--    Esta função é didática, pois mostra o uso de função de alta ordem
--    Contudo, o booleano não seria necessário se conseguíssemos fazer casamento 
--    de padrão com a função parâmetro-}
f4 :: Char -> (Int->Int->Int) -> [Int] -> Int
f4 c op x = f4_aux (isDigit c) op x


f4_aux :: Bool->(Int->Int->Int)->[Int]->Int
f4_aux True op [] = 0
f4_aux True op (a:b) = op a (f4_aux True op b)
f4_aux False op [] = 1
f4_aux False op (a:b) = op a (f4_aux False op b)


--Funções de alta ordem, exemplo com soma e subtração

sumLsubL::(Int->Int->Int)->[Int]->Int
sumLsubL op [] = 0
sumLsubL op (a:b) = op a (sumLsubL op b)

-- função map aplica uma função a cada elemento de uma lista
alteraL f [] = []
alteraL f (a:b) = f a : alteraL f b