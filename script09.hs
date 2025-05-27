import Data.Char

{- 01 qual é o resultado da computação desta implementação? -}

f1 :: [(Int,Char)]   
f1 = [ (a,b) | a <-[1..27], b<-['a'..'z']]

{- 02 Considere uma tupla (x, y) que aparece na lista gerada por f1. Faça uma função que remova toda ocorrência subsequentes de tuplas (x, _), mantendo apenas a primeira 
    exemplo: f2 [(1,'a'),(1,'b'),(1,'c'),(2,'d'),(2,'e'),(2,'f'),(2,'g')] retorna [(1,'a'),(2,'d')] 
    Observação: você não deve considerar o Char da tupla em sua computação-}

{- 03 Desafio: considere as repetições das tuplas (x,_) geradas por f1. Então, faça um filtro parecido com f2, contudo, para o primeiro Int, a primeira tupla com a ocorrência será mantida na solução. Por conseguinte, para o segundo Int, a segunda tupla será mantida, e assim por diante. Vejam o exemplo:
f3 [(1,'a'),(1,'b'),(1,'c'),(1,'d'),(2,'a'),(2,'b'),(2,'c'),(2,'d'),(3,'a'),(3,'b'),(3,'c'),(3,'d'),(4,'a'),(4,'b'),(4,'c'),(4,'d')] retorna [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
Observação: você não deve considerar o Char da tupla em sua computação -}

{- 04 Considere a função f4Fold como definida a seguir -}

f4Fold::(t->u->u)->[t]->u->u
f4Fold _ [] z    = z
f4Fold f (a:b) z = f a (f4Fold f b z)
 
{- implemente um exemplo de computação em que t diferente de u para a computação de f4Fold -}
