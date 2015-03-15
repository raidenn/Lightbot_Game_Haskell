----------------------------------------------------------------------------
--                               LIGHTBOT
----------------------------------------------------------------------------

{-| __TAREFA A__

O programa deve ler do stdin um ficheiro de texto e verificar se segue a especificação apresentada no enunciado. 
Como resultado, o programa deve imprimir no stdout uma única linha contendo:

    - __OK__, se o ficheiro estiver conforme a especificação;
    - __num__, no caso de erro (sendo num o número da linha onde ocorre o primeiro erro). 
-}

module Main where

import Data.Char
import Data.List

----------------------------------------------------------------------------
-- IO
----------------------------------------------------------------------------

main = do  

    inp <- getContents
    putStr (outStr (tarefaA (lines inp)))

----------------------------------------------------------------------------
-- TAREFA A
----------------------------------------------------------------------------

{-| A função 'tarefaA' executa a verificação do enunciado proposto na TAREFA A. Recebe uma lista de Strings, proveniente da função __lines__ que esta definida no Prelude. 
Esta vai devolver [__OK__] se a verificação for corresponder aos parametros testados, ou devolve o numero da linha onde ocorre o primeiro erro. Exemplo: [__2__].

>>> tarefaA ["aacdD","aacaa","bbCa","2 1 N","SEASLE"]
["3"]

>>> tarefaA ["a2cdD","aacaa","bbCaa","0 1 S","SEASLE"]
["1"]

>>> tarefaA ["aacdD","aacaa","bbCaa","0 1 S","SEASLE"]
["OK"]

>>> tarefaA ["aacdD","aacaa","bbCaa","0 1 S","SEASXXXLE"]
["5"] 

-}

tarefaA :: [String] -> [String]
tarefaA [] = ["1"]
tarefaA l = [val_inval]
 where 
       tabuleiro       = takeWhile validaLetras l 
       erroTabuleiro   = validaTabuleiro tabuleiro
       
       tamanhoX        = length (head l)
       tamanhoY        = length tabuleiro 
       
       coordenadas     = take 1 (dropWhile validaLetras l)
       erroCoordenadas = if validaCoordenada tamanhoX tamanhoY coordenadas == True then 0 
                         else length tabuleiro + 1
       
       comandos        = drop 1 (dropWhile validaLetras l)
       erroComandos    = if length comandos == 0 then length tabuleiro +2
                         else if length comandos > 1 then length tabuleiro + auxComandos comandos
                              else if validaComandos (comandos !! 0) then 0
                                   else length tabuleiro +2

       numErro 		   = if erroTabuleiro > 0 then erroTabuleiro
                                              else if erroCoordenadas > 0 then erroCoordenadas
                                                                          else erroComandos 

       val_inval       = if numErro > 0 then show numErro else "OK"


{-| A função 'validaTabuleiro' recebe a variavel __tabuleiro__ (na forma [String]) e devolve
um número inteiro. Este será 0 se passar todas as condições de verificação, ou __n__ se falhar em algum momento. 
__n__ é a linha onde o erro aconteceu. 

>>> validaTabuleiro ["aacdD","aacaa","bbCaa"]
0

>>> validaTabuleiro ["aacdD","aaaa","bbCa5a"]
2

>>> validaTabuleiro ["aacdD","aacaa","5bCaa"]
3 

-}

validaTabuleiro :: [String] -> Int
validaTabuleiro []    = 1
validaTabuleiro (h:t) = aux (length h) 1 (h:t)
	where aux x n []     = 0
	      aux x n (y:ys) = if x == 0 
	                       then n
	                       else if length y == x && validaLetras y == True 
	                            then aux x (n+1) ys
	                            else n

{- | A função 'validaLetras' vai verificar se os elementos da String são letras do alfabeto, sejas estas maiúsculas ou minúsculas.
 Devolve __True__ ou __False__ respetivamente.

>>>validaLetras "aacdD"
True

>>> validaLetras "5bCaa"
False

-}                             

validaLetras :: String -> Bool
validaLetras str = and (map isLetter str)

{- | A função 'validaCoordenada' vai validar as coordenadas da posição inicial do robot. Esta deve receber o tamanho do eixo xx do tabuleiro, o tamanho do eixo yy
do tabuleiro e a String em que a primeira e a segunda são numeros inteiros positivos e a terceira corresponde à orientação 
do robot e apenas pode assumir as letras "N","S", "E",ou "O" (ex:"1 1 N"). Será retornado o valor __True__ se as coordenadas forem válidas e apontarem para uma posição
do tabuleiro, ou __False__ se uma destas condições não for verificada.

>>> validaCoordenada 8 4 ["0 0 S"]
True

>>> validaCoordenada 8 4 ["8 3 N"]
False

>>> validaCoordenada 8 4 ["1 4 E"]
False

>>> validaCoordenada 8 4 ["-1 3 E"]
False

-}

validaCoordenada :: Int -> Int -> [String] -> Bool
validaCoordenada x y []    = False
validaCoordenada x y (h:t) =  if length (words h) == 3
                              then if (last (words h) == "N" || last (words h) == "S" || last (words h) == "E" || last (words h) == "O") 
                                       && (validaNumeros (head (words h)) == True) 
                                       && (validaNumeros (head(tail(words h))) == True)
                                       && (((read (head (words h)))) <= (x-1))
                                       && (((read (head (tail(words h))))) <= (y-1))
                                   then True 
                                   else False
                              else False

{- | A função 'validaNumeros' vai verificar se os elementos da String são numeros inteiros e positivos. Devolve __True__ ou __False__ respetivamente.

>>>validaNumeros "1"
True

>>> validaNumeros "Q"
False

>>> validaNumeros "6x"
False

-}

validaNumeros :: String -> Bool 
validaNumeros []    = True
validaNumeros (h:t) = ((isDigit h) && (digitToInt h >= 0)) && validaNumeros t

{-| A função 'validaComandos' verifica se a programação dada ao robot é válida, devolvendo um __True__ ou __False__ respetivamente. Esta pode apenas conter as letras
"L", "S", "A", "D", "E".


>>> validaComandos "SEASLE"
True

>>> validaComandos "SEASLE A"
False

-}

validaComandos :: String -> Bool 
validaComandos [] = False
validaComandos l  = aux l 
    where aux []    = True
          aux (h:t) = if (h `elem` "LSADE") == True 
                      then aux t 
                      else False

{- |'auxComandos' é uma função auxiliar, que verifica se existem mais linhas depois da linha dos comandos. Devolve o numero 2 ou 3, consoante a verificação.

>>> auxComandos ["SEASLE","SEASLE"]
3

>>> auxComandos ["SEA3SLE","SEASLE"] 
2

-}

auxComandos :: [String] -> Int
auxComandos (c:cs) = if validaComandos c == True then 3 else 2

{-| A função 'outStr' recebe uma [String] e devolve uma String. Esta função
serve para apresentar o resultado da função 'tarefaA', 'tarefaB', 'tarefaC', entre outras aplicações. 

>>> outStr ["123"]
"123"

>>> outStr ["OK"]
"OK"

-}

outStr :: [String] -> String
outStr [] = "\n"
outStr t  = unlines t



----------------------------------------------------------------------------
-- TESTES LOCAIS
----------------------------------------------------------------------------

{-| A função 'testaA' é usada para testar localmente a verificação da tarefa A. Recebe uma [String] com os numeros dos tabuleiros que vão ser 
testados (sem a sua extensão que é por definição .in). Vai o resultado esperado ( lido de um ficheiro com extensão .out), e o resultado da tarefaA. Se ambos coincidirem, 
este imprime __Resultado tarefa A ------>__, se não, imprime __FALHOU NA TAREFA A _-_-_-> __.

>>> testaA ["00"]
Teste a verificar: 00
Resultado esperado-----> OK
Resultado tarefa ------> OK

>>> testaA ["00","01"]
Teste a verificar: 00
Resultado esperado-----> OK
Resultado tarefa ------> OK
Teste a verificar: 01
Resultado esperado-----> 4
Resultado tarefa ------> 4

-}

testaA :: [String] -> IO ()
testaA []    = putStrLn "\nAcabou!!!!!!!!!!!!!!!!!\n"
testaA (h:t) = do inp <- readFile ("../tests/tarefaA/teste" ++ h ++".in")
                  resultOut <- readFile("../tests/tarefaA/teste" ++ h ++".out")
                  result <-return (outStr(tarefaA (lines inp)))
                  conteudoStr <- return ((lines inp) :: [String])
                  putStrLn ("Teste a verificar: " ++ h)
                  if (result == resultOut)
                  then putStrLn (("Resultado esperado ------> "++resultOut) ++ "Resultado tarefa A ------> " ++ result)
                  else putStrLn (("RESULTADO ESPERADO _-_-_-> "++resultOut) ++ "FALHOU NA TAREFA A _-_-_-> " ++ result)
                  testaA t

{-| A função 'a' contem apenas uma listagem de testes da tarefa A.

>>> a = ["00", "01", "02", "03", "04", "05", "06", "07", "08", "09","10", "11", "12", "13", "14", "15", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29","30"]

-}

-- Testes tarefa A
a :: [String]
a = ["00", "01", "02", "03", "04", "05", "06", "07", "08", "09","10",
     "11", "12", "13", "14", "15","20", "21", "22", "23", "24", "25",
     "26", "27", "28", "29","30"]