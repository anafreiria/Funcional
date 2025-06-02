import Data.Char

{- 01 qual é o resultado da computação desta implementação? -}

f1 :: [(Int,Char)]   
f1 = [ (a,b) | a <-[1..26], b<-['a'..'z']]

{- 02 Considere uma tupla (x, y) que aparece na lista gerada por f1. Faça uma função que remova toda ocorrência subsequentes de tuplas (x, _), mantendo apenas a primeira 
    exemplo: f2 [(1,'a'),(1,'b'),(1,'c'),(2,'d'),(2,'e'),(2,'f'),(2,'g')] retorna [(1,'a'),(2,'d')] 
    Observação: você não deve considerar o Char da tupla em sua computação-}

f2_v1 :: [(Int, Char)]->Bool->[(Int,Char)]
f2_v1 [] _      = []
f2_v1 [a] insere 
  |insere    = [a]
  |otherwise = []
f2_v1 (a:b:x) insere
  |insere          = a : f2_v1 (a:b:x) (False)
  |fst a /= fst b  = b : f2_v1 (b:x)   (False)
  |otherwise       =     f2_v1 (b:x)   (False) 

{- solução João Pedro -}
f2_jp :: [(Int, Char)]->[(Int,Char)]
f2_jp [a] = [a]
f2_jp ((i1,c1):(i2,c2):x)
  |i1 == i2  = f2_jp ((i1,c1):x)
  |otherwise = (i1,c1):f2_jp ((i2,c2):x) 
f2_jp [] =[]

{------ outra solução usando a geração de listas de lista -}

{- cria uma lista com tuplas de mesmo Int -}
f2_frag::[(Int,Char)]->Int-> [(Int,Char)]
f2_frag [] _     = []
f2_frag (a:x) n
   |fst (a) == n = a:f2_frag x n
   |otherwise    =   f2_frag x n

{- cria lista de todos os Int que aparecem nas tuplas -}   
f2_ListInts::[(Int,Char)]->[Int]
f2_ListInts l = [a|(a,b)<-l]

{- remove Int repetidos em [Int] -}
f2_ListIntsfilter::[Int]->[Int]
f2_ListIntsfilter [] =[]
f2_ListIntsfilter (a:b) = a:f2_ListIntsfilter (f2_limpa a b)

{- auxiliar de f2_ListIntsfilter -}
f2_limpa :: Int->[Int]->[Int]
f2_limpa _ []    = []
f2_limpa n (a:b) 
  |n==a      = f2_limpa n b
  |otherwise = a:f2_limpa n b
     
{- gera lista de tuplas com mesmo Int -}
f2_ger ::[(Int,Char)]->[Int]-> [[(Int,Char)]]
f2_ger [] _ = []
f2_ger _ [] = []
f2_ger ld (a:x) = f2_frag ld a: f2_ger ld x

{- interface para gerar lista de listas de duplas -}
f2_gera ::([(Int,Char)])-> [[(Int,Char)]]
f2_gera f_imput = f2_ger (f_imput) (f2_ListIntsfilter (f2_ListInts f_imput))
  
{- função temporária para gerar f2_v2 -}
f2_vtemp::[[(Int,Char)]]->[(Int,Char)]
f2_vtemp  []        = []
f2_vtemp  ((a:b):x) = a:f2_vtemp x
 
{- função principal que recebe f1 como parâmetro -} 
f2_v2::([(Int,Char)])-> [(Int,Char)]
f2_v2 f_imput = f2_vtemp (f2_gera f_imput)  

{- 03 Desafio: considere as repetições das tuplas (x,_) geradas por f1. Então, faça um filtro parecido com f2, contudo, para o primeiro Int, a primeira tupla com a ocorrência será mantida na solução. Por conseguinte, para o segundo Int, a segunda tupla será mantida, e assim por diante. Vejam o exemplo:
f3 [(1,'a'),(1,'b'),(1,'c'),(1,'d'),(2,'a'),(2,'b'),(2,'c'),(2,'d'),(3,'a'),(3,'b'),(3,'c'),(3,'d'),(4,'a'),(4,'b'),(4,'c'),(4,'d')] retorna [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
Observação: você não deve considerar o Char da tupla em sua computação -}

{- função temporária para gerar f2_v2 -}
f3_vtemp::[[(Int,Char)]]->Int->[(Int,Char)]
f3_vtemp  []  _          = []
f3_vtemp  (a:x) contador = (encontra a (contador)) :f3_vtemp x (contador +1)

encontra ::[(Int,Char)]->Int->(Int,Char)
encontra (a:b) contador
  |contador == 0 = a
  |otherwise     = encontra b (contador-1)
  
{- chamada principal para f3 -}
f3:: ([(Int,Char)])->[(Int,Char)]
f3 f_imput = f3_vtemp (f2_gera f_imput) 0


{- 04 Considere a função f4Fold como definida a seguir -}

f4Fold::(t->u->u)->[t]->u->u
f4Fold _ [] z    = z
f4Fold f (a:b) z = f a (f4Fold f b z)
 
{- implemente um exemplo de computação em que t diferente de u para a computação de f4Fold -}

{- sugestão Eliseu -}
temPar::Int->Bool->Bool
temPar x b = (mod x 2 == 0) || b

todosPar::Int->Bool->Bool
todosPar x b = (mod x 2 == 0) && b
 
generalizaPar:: (Bool->Bool->Bool)->Int->Bool->Bool
generalizaPar f x b = f (mod x 2 == 0) b

{- solução do Davi -}
f_d :: Char->Char->Bool->Bool
f_d     c1 c2 b = c1 == c2 || b

f_p :: [Char] -> Char -> Bool
f_p      l  c = f4Fold (f_d c) l False













