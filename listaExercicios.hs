import Data.Char  (isLower, toUpper, ord, chr, isDigit, digitToInt)
--import Data.Complex (Complex((:+)), sqrt)

{-1. Declare, em Haskell, as
funções abaixo, contemplando, também, os protótipos (cabeçalhos):-}

f1 :: Float -> Float
f1 x
    |x >= 0 = (x+4)/(x+2)
    |otherwise = 2/x

f2 :: Float -> Float -> Float
f2 x y
    |x >= y = x + y
    |otherwise = x - y


f3 :: Float -> Float -> Float -> Float
f3 x y z
    |(x+y) > z = x + y + z
    |(x+y) < z = x - y - z
    |otherwise = 0

{-2 Localize, explique e corrija o erro na função que deve calcular o fatorial de um número, como
se segue:-}

{- Essa definição está correta em termos de sintaxe, mas logicamente está incompleta. Ela define a recursão,
 mas não define quando ela deve parar. Isso faz com que a função entre em loop infinito (ou stack overflow) 
 para qualquer valor de entrada.-}

fat :: Int -> Int
fat 0 = 1 
fat n = n * fat (n-1)

{- 3 Considere a função em Haskell soma::Int->Int->Int que retorna a soma entre os dois parâ-
metros. Assim, faça uma função em Haskell que resulte a multiplicação de dois parâmetros
fazendo uso da função soma. -}

soma :: Int -> Int -> Int
soma a b = a + b


multiplicacao :: Int -> Int -> Int
multiplicacao _ 0 = 0
multiplicacao x y
  | y > 0     = soma x (multiplicacao x (y - 1))
  | y < 0     = negate (multiplicacao x (negate y))


{- 4. Escreva, em Haskell, a função invertInt::Int->Int que inverta os dígitos de um número inteiro.-}

invertInt :: Int -> Int
invertInt n = read (reverse (show (abs n))) * signum n

{-Vamos decompor essa função para entender o que está acontecendo:

abs n: Primeiro, pegamos o valor absoluto do inteiro de entrada n. Isso garante que lidemos com a inversão dos dígitos sem nos preocuparmos com o sinal negativo inicialmente.

show (abs n): Convertemos o valor absoluto do inteiro para uma String. Por exemplo, se n for -123, abs n será 123, e show (abs n) resultará em "123".

reverse (show (abs n)): A função reverse da biblioteca padrão de Haskell inverte a ordem dos caracteres na string. No nosso exemplo, "123" se torna "321".

read (reverse (show (abs n))): A função read tenta analisar uma string e convertê-la para um valor do tipo esperado (neste caso, Int devido à assinatura da função). Assim, "321" é convertido de volta para o inteiro 321.

signum n: Esta função retorna o sinal do número original n. Retorna 1 se n for positivo, -1 se for negativo e 0 se for zero.

* signum n: Finalmente, multiplicamos o inteiro invertido (o valor absoluto invertido) pelo sinal do número original. Isso garante que o sinal do número seja preservado no resultado final. Se o número original era -123, o resultado será 321 * (-1), que é -321-}

invertInt1 :: Int -> Int
invertInt1 n
  | n < 0     = - (invertInt1 (abs n))
  | otherwise = read (reverse (show n)) :: Int


{-5.Escreva, em Haskell, a definição de uma função fourPower que retorne o seu argumento elevado
à quarta potência. Use a função square dada em sala de aula na definição de fourPower.-}

square :: Int -> Int
square x = x * x

fourPower :: Int -> Int
fourPower z = square (square z)

{-7.Escreva, em Haskell, uma função que informa de quantas maneiras é possível escolher n objetos
em uma coleção original de m objetos, para m ≥ n.
Vamos utilizar a função fatorial para isso de acordo com a formula matematica para combinações-}

combinacao :: Int -> Int -> Int
combinacao m n
    | m >= n = fat m `div` (fat n * (fat (m - n)))
    | otherwise = error "m deve ser maior ou igual a n"

{-8.Escreva uma função, em Haskell, que calcule o MDC de maneira recursiva, nesse caso dde acordo com o algoritmo de Euclides.-}
mdc :: Int -> Int -> Int
mdc m n
  | m `mod` n == 0 = n -- Caso base: se o resto da divisão de m por n for 0, então n é o MDC
  | otherwise = mdc n (m `mod` n) -- Chamada recursiva: chamamos a função mdc com n e o resto da divisão de m por n


{-9. Escreva, em Haskell, uma função que retorna quantos múltiplos de um determinado inteiro tem
em um intervalo fornecido. Por exemplo, o número 4 tem 2 múltiplos no intervalo de 1 a 10.
howManyMultiples 4 1 10 = 2.-}

-- Vamos resolver isso usando compreensão de listas e a função length para contar os múltiplos.
intervaloMultiples :: Int -> Int -> Int -> Int
intervaloMultiples x a b = length [i | i <- [a..b] , i `mod` x == 0]


{-10. Escreva, em Haskell, uma função que retorna o último dígito de um número inteiro.-}

ultimoDigito :: Int -> Int
ultimoDigito n = abs n `mod` 10


{-11. Escreva, em Haskell, uma função que retorna o dígito de um número inteiro de acordo com a
posição informada.-}



anyDigit :: Int -> Int -> Int
anyDigit pos n
  | pos < 0 = -1
  | pos >= numDigits n = -1
  | otherwise = (abs n `div` 10^(numDigits n - pos - 1)) `mod` 10

numDigits :: Int -> Int
numDigits n
  | abs n < 10 = 1
  | otherwise = 1 + numDigits (abs n `div` 10)


{-(a) O que está errado nessa definição?
(b) Especifique corretamente uma função allDifferent para o propósito necessário.-}

allDiferent :: Int -> Int -> Int -> Bool
allDiferent a b c 
      | a /= b && b /= c && a /= c = True
      | otherwise = False

{-13. Escreva uma função howManyEqual que retorne quantos dos três números inteiros fornecidos
como argumentos são iguais. A resposta poderá ser 3 (todos iguais), 2 (dois iguais e o terceiro
diferente) ou 0 (todos diferentes).-}

quantoIguais :: Int -> Int -> Int -> Int
quantoIguais a b c
  | a == b && b == c = 3
  | a == b || b == c || a == c = 2
  | otherwise = 0


{-14.a)Implemente a função howManyLess que calcule quantos dias as vendas foram inferiores
a um dado valor, dentro de um intervalo de dias dentro do período total. O primeiro
parâmetro de howManyLess indica o valor mínimo de vendas, o segundo parâmetro indica
o dia do início do intervalo e o terceiro parâmetro é o dia do fim do intervalo desejado
dentro do período total de dias da função;-}

vendas :: Int -> Int
vendas 1 = 41
vendas 2 = 0
vendas 3 = 0
vendas 4 = 2
vendas 5 = 0
vendas 6 = 55
vendas 7 = 30
vendas _ = 0

vendasInf :: Int -> Int -> Int -> Int
vendasInf v i s
    | i > 7 = error "Número de vendas inválido"
    | i == s = 0
    | vendas i <= v = 1 + vendasInf v (i+1) s    
    | otherwise = vendasInf v (i+1) s

{-b) Implemente a função noZeroInPeriod::Int->Bool que retorna True somente se não há
nenhum dia no período em que o número de vendas da função sales foi zero.-}

noZeroInPeriod::Int->Bool
noZeroInPeriod 0 = True
noZeroInPeriod i
    |i > 7 = error "Número de vendas inválido"
    |vendas i == 0 = False
    |otherwise = noZeroInPeriod (i-1)

{-(c) Implemente a função zerosInPeriod::[Int] que retorne a lista de todos os dias em que as
vendas foram de zero unidades;-}
zerosInPeriod::[Int]
zerosInPeriod = [i | i <- [1..7], vendas i == 0]

{- (d) Utilizando listas de inteiros, retorne os dias em que as vendas foram abaixo de um deter-
minado valor passado como parâmetro; -}

vendasAbaixo :: Int -> [Int]
vendasAbaixo v = [i | i <- [1..7], vendas i < v]

{-16. Escreva uma definição equivalente à exibida abaixo, mas usando apenas uma única cláusula
em casamento de padrão:
funny x y z
| x > z = True
| y >= x = False
| otherwise = True-}

funny :: Int -> Int -> Int -> Bool
funny x y z = x > z || y < x

{-17. Implemente uma função que converte uma letra minúsculas como entrada para seu equivalente
em maiúsculo. Caso a entrada não seja uma letra minúscula, retorne o próprio caractere
de entrada. Como dica, veja a função predefinida isLower::Char->Bool.-}

converte :: Char -> Char
converte c
  | isLower c = chr (ord c - ord 'a' + ord 'A')
  | otherwise = c 

-- ou entao
converte1 :: Char -> Char
converte1 c
  | isLower c = toUpper c
  | otherwise = c

{-18. Defina uma função charToNum::Char->Int que converte um dígito numérico do tipo Char
(como ´3´) para o valor que ele representa em Int, (3). Se o caractere de entrada não representa
um dígito numérico, a função deve retornar -1.-}
charToInt :: Char -> Int 
charToInt c
    |isDigit c = ord(c) - ord('0')
    |otherwise = -1

{-19. Implemente a função duplicate::String ->Int->String que recebe uma string s e um número
inteiro n. A função deve retornar a concatenação de n cópias de s. Se n for zero, retorna .
Como dica, usar o operador de concatenação pré-definido (++)::String->String->String.-}

duplicate :: String -> Int -> String
duplicate _ 0 = ""
duplicate xs n = xs ++ duplicate xs (n-1)

{-20. Implemente a função pushRight::String->Int->String que recebe uma string s e um número
inteiro n e retorna uma nova string t com k caracteres ’>’ inseridos no início de s. O valor de
k deve ser tal que o comprimento de t seja igual a n. Obs: se n é menor que o comprimento
de s, a função retorna a própria string s.-}
colocandoMaior :: String -> Int -> String
colocandoMaior xs n
    |length xs > n = xs
    |otherwise = replicaChar (n - length xs) '>' ++ xs

replicaChar :: Int -> Char -> String
replicaChar 0 _ = []
replicaChar i c = c : replicaChar (i-1) c

{-22. Faça em Haskell uma solução para inverter os elementos de uma lista de Inteiros.-}

inverteListaInt :: [Int] -> [Int]
inverteListaInt [] = []
inverteListaInt [a] = [a]
inverteListaInt (a:ax) = inverteListaInt ax ++ [a]

{-23. Faça em Haskell uma solução para, dada uma lista de inteiros, retornar uma dupla de listas de
inteiros onde a primeira conterá os elementos ímpares e a segunda os elementos pares passados
como parâmetro-}

separa :: [Int] -> ([Int],[Int])
separa [] = ([],[])
separa (a:ax)
    |odd a = (a:impar, pares)
    |otherwise = (impar, a:pares)
    where(impar, pares) = separa ax
    
{-24. Faça em Haskell uma solução para, dada uma lista de inteiros, retornar a string contendo as
letras do alfabeto cuja posição é dada pelos elementos da lista-}
chrLista :: [Int] -> String
chrLista [] = ""
chrLista (a:ax) = chr a : chrLista ax

{-25. Sabendo que [1..7] é equivalente à lista [1,2,3,4,5,6,7], complete as correspondências abaixo:
(a) [’a’..’g’] = "abcdefg"
(b) [0.1 ..0.9] =
(c) [0.1,0.3 .. 0.9] = [0.1,0.3,0.5,0.7,0.8999999999999999]
(d) [0.1,0.3 ..1.8] = [0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9]
(e) [0.2,0.4 ..0.8] = [0.2,0.4,0.6,0.8]
(f) [1,4..15]=[ 1,4,7,10,13]
-}

{-26.Faça em Haskell uma solução para o seguinte problema: Dada uma lista de caracteres [Char],
e um caractere a, retornar quantos caracteres da lista são iguais a a.-}

contaChar :: String -> Char -> Int
contaChar [] _ = 0
contaChar (a:ax) n
    | a == n = 1 + contaChar ax n
    |otherwise = contaChar ax n


{-27. Para uma lista de elementos inteiros ordenada qualquer, faça uma função que retorne uma lista
de inteiros ordenada sem elementos repetidos-}

purifica :: [Int] -> [Int]
purifica [] = []
purifica [a] = [a]
purifica (a:b:xs)
    |a == b = purifica (b:xs)
    |otherwise = a : purifica (b:xs)


{-28. . Faça uma solução em Haskell que, dada uma lista de inteiros, ela retorne uma lista com uma
repetição de cada elemento de acordo com seu valor.-}

repetir :: [Int] -> [Int]
repetir [] = []
repetir (a:ax) = replicar a a ++ repetir ax


replicar :: Int -> Int -> [Int]
replicar _ 0 = []
replicar n v = n : replicar n (v-1)

{-29. Faça uma solução em Haskell que, dada uma lista de caracteres maiúsculos, ela retorne uma
lista com uma repetição de cada elemento de acordo com o valor de sua ordem no alfabeto.
{-exemplo-}
Main> proliferaChar [C,B,D] = "CCCBBDDDD"-}

proliferaChar :: String -> String
proliferaChar [] = []
proliferaChar (x:xs) = replica (ord x - ord 'A' + 1) x ++ proliferaChar xs

replica :: Int -> Char -> String
replica 0 _ = []
replica n c = c : replica (n-1) c

{-30. Defina uma função que retorne uma tupla-3(tripla) contendo o caractere fornecido como en-
trada, o mesmo caratere em letras minúsculo ou maiúsculas, e o seu número da tabela ASCII.-}


charInfo :: Char -> (Char, Char, Int)
charInfo c
  | ord c >= 65 && ord c <= 90 = (c, chr (ord c + 32), ord c)  -- se maiúscula, vira minúscula
  | ord c >= 97 && ord c <= 127 = (c, chr(ord c - 32), ord c)
  |otherwise = error "tem do"                                  -- se já for minúscula ou outro


{-31. Seja o cadastro de pessoas dado pela função a seguir:-}

pessoa :: Int -> (String, Int, Char)
pessoa rg
  | rg == 1 = ("Joao Silva", 12, 'm')
  | rg == 2 = ("Jonas Souza", 51, 'm')
  | rg == 3 = ("Jocileide Strauss", 21, 'f')
  | otherwise = ("Não há ninguém mais", 9999, 'x')

-- (a) O nome da pessoa de menor idade até um determinado registro.
menorIdade :: Int -> String
menorIdade 0 = ""
menorIdade n = buscaMenor [pessoa i | i <- [1..n]]

buscaMenor :: [(String, Int, Char)] -> String
buscaMenor [(nome, _, _)] = nome
buscaMenor ((nome1, idade1, _):(nome2, idade2, _):xs)
  | idade1 <= idade2 = buscaMenor ((nome1, idade1, 'x') : xs)
  | otherwise        = buscaMenor ((nome2, idade2, 'x') : xs)


-- (b) A idade média de todas as pessoas até um dado registro.)
idadeMedia :: Int -> Float
idadeMedia 0 = 0
idadeMedia n = fromIntegral (somaIdades [pessoa x | x <- [1..n]]) / fromIntegral n
--FromIntegral é usado para converter o resultado da soma e o número de registros para Float, permitindo a divisão correta.

somaIdades :: [(String, Int, Char)] -> Int
somaIdades [] = 0
somaIdades ((_, idade, _):xs) = idade + somaIdades xs


--(c) O número de pessoas do sexo masculino.
numMasculino :: Int -> Int
numMasculino 0 = 0
numMasculino n = somaSexo [pessoa x | x <- [1..n]] 'm'

somaSexo :: [(String, Int, Char)] -> Char -> Int
somaSexo [] _ = 0
somaSexo ((_,_,sexo):xs) s
  |sexo == s = 1 + somaSexo xs s
  |otherwise = somaSexo xs s 

--(d) O número de pessoas do sexo feminino.
numFeminino :: Int -> Int 
numFeminino 0 = 0
numFeminino n = somaSexo [pessoa x | x <- [1..n]] 'f'

{-32. Construa uma função em Haskell que recebe 4 inteiros e devolve uma tupla-4 com os quatros
valores originais, só que ordenados.-}

ordena4 :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
ordena4 a b c d = paraTupla (ordenaLista[a, b, c, d])

ordenaLista :: [Int] -> [Int]
ordenaLista [] = []
ordenaLista (x:xs) = insere x (ordenaLista xs)

insere :: Int -> [Int] -> [Int]
insere x [] = [x]
insere x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insere x ys

paraTupla :: [Int] -> (Int, Int, Int, Int)
paraTupla [a,b,c,d] = (a,b,c,d)
paraTupla _ = error "Lista não tem 4 elementos"



{-33. Dadas duas datas (d1 , m1 , a1 ) e (d2 , m2 , a2 ), tal que data1 ≤ data2 , construa uma função que
retorne quantos dias existem entre estas duas datas , onde di define o dia do mês mj no ano ak .-}

diasEntreDatas :: (Int, Int, Int) -> (Int, Int, Int) -> Int
diasEntreDatas (d1, m1, a1) (d2, m2, a2) = abs (diasTotal (d2, m2, a2) - diasTotal (d1, m1, a1))

-- Calcula o número total de dias desde 01/01/0000 até a data dada
diasTotal :: (Int, Int, Int) -> Int
diasTotal (d, m, a) = d + sum (take (m-1) diasPorMesAno) + (a * 365) + anosBissextosAntes a
  where
    diasPorMesAno
      | ehBissexto a = [31,29,31,30,31,30,31,31,30,31,30,31]  -- Fevereiro tem 29 dias
      | otherwise    = [31,28,31,30,31,30,31,31,30,31,30,31]  -- Fevereiro tem 28 dias

-- Verifica se o ano é bissexto
ehBissexto :: Int -> Bool
ehBissexto ano = (ano `mod` 4 == 0 && ano `mod` 100 /= 0) || (ano `mod` 400 == 0)

-- Conta quantos anos bissextos existiram antes do ano dado
anosBissextosAntes :: Int -> Int
anosBissextosAntes ano = (ano - 1) `div` 4 - (ano - 1) `div` 100 + (ano - 1) `div` 400

{-34. Crie uma função que receba os coeficientes de uma equação do segundo grau ax2 + bx + c = 0
na forma (a,b,c) e retorne as raízes imaginárias, indicando um erro.-}



-- raizesImaginarias :: (Floating a, Ord a) => (a, a, a) -> (Complex a, Complex a)
-- raizesImaginarias (a, b, c)
--     | a == 0    = error "Erro: não é uma equação do segundo grau (a = 0)."
--     | delta >= 0 = error "As raízes não são imaginárias."
--     | otherwise = (raiz1, raiz2)
--   where
--     delta = b^2 - 4*a*c
--     sqrtDelta = sqrt (delta :+ 0)
--     doisA = 2 * a
--     raiz1 = ((-b) :+ 0 + sqrtDelta) / (doisA :+ 0)
--     raiz2 = ((-b) :+ 0 - sqrtDelta) / (doisA :+ 0)


{-35. Construa uma função que, dado três valores, verifique se os mesmos podem ser os lados de
um triângulo. Se for possível formar o triângulo, retorne uma tupla-2 com o tipo do triângulo
formado (com relação às arestas) e o perímetro do mesmo.-}

triangulo :: (Float, Float, Float) -> (String, Float)
triangulo (a,b,c)
    | a + b > c && a + c > b && b + c > a = (tipoTriangulo, perimetro)
    |otherwise = ("0", 0.0)
    where
        perimetro = a + b + c
        tipoTriangulo
            | a == b && b == c = "Equilatero"
            | a == b && b /= c || a /= b && b == c || a /= b && c ==a  = "Isoceles"
            | a /= b && b /= c && a /= c = "Escaleno"


{-36. Apresentada a base de dados de 10 professores:-}
base :: Int -> (Int, String, String, Char)
base x
    | x == 0 = (1793, "Pedro Paulo", "MESTRE", 'M')
    | x == 1 = (1797, "Joana Silva Alencar", "MESTRE", 'M')
    | x == 2 = (1534, "João de Medeiros", "DOUTOR", 'F')
    | x == 3 = (1267, "Cláudio César de Sá", "DOUTOR", 'M')
    | x == 4 = (1737, "Paula de Medeiros", "MESTRE", 'F')
    | x == 5 = (1888, "Rita de Matos", "MESTRE", 'F')
    | x == 6 = (1267, "Cláudio César de Sá", "DOUTOR", 'M')
    | x == 7 = (1737, "Paula de Medeiros", "MESTRE", 'F')
    | x == 8 = (1888, "Rita de Matos", "MESTRE", 'F')
    | x == 9 = (1698, "Tereza Cristina Andrade", "MESTRE", 'F')
    | x == 10 = (0, "", "", '0')

-- (a) O número de doutores na base.



titulo :: Int -> String
titulo x = case base x of (_, _, t, _) -> t

numDoutores :: Int
numDoutores = length [x | x <- [0..9], titulo x == "DOUTOR"]


-- (b) O número de mulheres.

mulieres :: Int -> Char
mulieres x = case base x of (_,_,_, s) -> s

numMulieres :: Int
numMulieres = length [x | x <- [0..9], mulieres x == 'F']

-- (c) O número de mestres do sexo masculino.

numMestresMasculinos :: Int
numMestresMasculinos = length [x | x <- [0..9], let (_, _, t, s) = base x, t == "MESTRE" && s == 'M']

--(d) O nome do professor mais antigo (número de menor matrícula)

nomeMaisAntigo :: String
nomeMaisAntigo = snd3 (base indiceMaisAntigo)
  where
    matriculas = [(x, fst4 (base x)) | x <- [0..9]]
    indiceMaisAntigo = fst (minimumBy (\(_, m1) (_, m2) -> compare m1 m2) matriculas)

-- Funções auxiliares para extrair elementos da tupla
fst4 (a, _, _, _) = a
snd3 (_, b, _, _) = b
