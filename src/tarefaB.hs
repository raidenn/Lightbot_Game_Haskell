---------------------------------------------------------------------------
--                               LIGHTBOT
----------------------------------------------------------------------------

{-| __TAREFA B__

O programa deve ler do stdin um ficheiro de texto com a descrição do tabuleiro; o estado inicial do /robot/;
e a sequência de comandos (conforme formato especificado no enunciado). Deve assumir que o formato dos dados de
entrada é válido. Como resultado, o programa deve imprimir no stdout:

    - __ERRO__, se o primeiro comando não for aplicável (e.g. tentar executar o comando __L__ sobre uma posição sem lâmpada);
    - __xcoord ycoord orient__, no caso do comando ser aplicável (onde xcoord, ycoord e orient denotam respectivamente as 
      coordenadas x e y do robot, e orient a sua orientação.
-}

module Main where

import Data.Char
import Data.List

data Orientacao = S | N | E | O deriving (Show)

type Tabuleiro = [String]
type Coordenada = (Int,Int)
type Comandos = String
type PosicaoRobot = (Int,Int,Char)
type TamanhoTabuleiro = (Int,Int)
type PosicaoTabuleiro = (PosicaoRobot,TamanhoTabuleiro)
type Nivel = Char
type LinhaTabuleiro = String
type LampadasAcesas = Int
type LampadasTabuleiro = Int
type ComandosExe = Int
type LampadasApagadas = [String]

----------------------------------------------------------------------------
-- IO
----------------------------------------------------------------------------

main = do  

    inp <- getContents
    putStr (outStr (tarefaB (lines inp)))

----------------------------------------------------------------------------
-- TAREFA B
----------------------------------------------------------------------------
   
{-| A função 'tarefaB' executa a verificação do enunciado proposto na TAREFA B. Recebe uma lista de Strings, correspondete ao Input e vai verificar se o primeiro comando da lista de comandos é valido ou não.
Devolve uma PosiçãoRobot (Int,Int,Char), sobe forma de [String], se o comando for válido ou __ERRO__ se esse comando não for válido.

>>> tarefaB ["aacdD","aacaa","bbCa","2 1 N","SEASLE"]
["ERRO"]

>>> tarefaB ["aacdD","aacaa","bbCaa","0 1 S","SEASLE"]
["0 0 S"]

-}

tarefaB :: [String] -> [String]
tarefaB l = [val_inval]
  where 
        tabuleiro   = takeWhile validaLetras l 
        coordenadas = take 1 (dropWhile validaLetras l)
        comandos    = head(drop 1 (dropWhile validaLetras l))
       
        tamTab      = tamTabuleiro tabuleiro
        pInicial    = posInicial coordenadas
       
        proxPosicao = if comandos == ""
       	              then (-1,-1,'N')
       	              else execComando tabuleiro (pInicial,tamTab) (head comandos)

        val_inval   = if proxPosicao == (-1,-1,'N') || proxPosicao == (-1,-1,'S') || proxPosicao == (-1,-1,'E') || proxPosicao == (-1,-1,'O')
       		  	        then "ERRO"
       			          else  if validaPosTab (proxPosicao,tamTab) == True
       			   	            then outPosRob proxPosicao
       			   	            else "ERRO"

{- | A função 'validaLetras' vai verificar se os elementos da String são letras do alfabeto, sejas estas maiúsculas ou minúsculas.
 Devolve __True__ou __False__ respetivamente.

>>>validaLetras "aacdD"
True

>>> validaLetras "5bCaa"
False

-}                             

validaLetras :: String -> Bool
validaLetras str = and (map isLetter str)

{-| A função 'outPosRob' serve para converter numa String o tipo PosicaoRobot ((Int,Int,Char))

>>> outPosRob (1,1,'N')
"1 1 N"

-}

outPosRob :: PosicaoRobot -> String
outPosRob (a,b,c) = show a ++ " " ++ show b ++ " " ++ [c]

{-| A função 'posInicial' serve para verificar a posição inicial do robot. Recebe uma [String] com as cordenadas e vai devolver essas mesmas coordenadas no 
formato (Int,Int,Char)

>>> posInicial ["1 1 N"] -> (Int,Int,Char)
(1,1,'N')

-}

posInicial :: [String] -> PosicaoRobot
posInicial (h:t) = (posX,posY,orient)
  where
        posX   = read(head(words h))
        posY   = read(head(tail(words h)))
        orient = head(last(words h))

{-| A função 'tamTabuleiro' serve para verificar a o tamanho do tabuleiro. Recebe uma [String] com as linhas do tabuleiro e vai 
devolver o seu tamanho como forma de (Int,Int)

>>> tamTabuleiro ["aacdD","aacaa","bbCaa"]
(5,3)

-}

tamTabuleiro :: Tabuleiro -> TamanhoTabuleiro -- [String] --> (Int,Int)
tamTabuleiro (h:t) = (tamX,tamY)
  where
        tamX = length (head (h:t)) 
        tamY = length (h:t)

{-| A função 'validaPosTab' serve para verificar verificar se uma determinada posição pertence a um determinado tabuleiro. 
Recebe a coordenada em questão e o tamanho do tabuleiro como forma de ((Int,Int,Char),(Int,Int)), respetivamente. Devolve __True__ ou __False__.

>>> validaPosTab ((1,1,'N'),(3,3))
True

>>> validaPosTab ((4,4,'N'),(3,3))
False

-}

validaPosTab :: PosicaoTabuleiro -> Bool
validaPosTab ((x,y,o),(xx,yy)) = if x <= (xx-1) && y <= (yy-1) && x >= 0 && y >= 0
                                 then True
                                 else False

{-| A função 'verificaNivel' serve para verificar verificar o nivel de uma determinada coordenada. 
Este recebe uma [String] com o tabuleiro e a coordenada que queremos verificar sob forma de (Int,Int). Vai devolver um __Char__ com  nivel em que se encontra.


>>> verificaNivel ["abc","def","hjy"] (2,2)
'c'

>>> verificaNivel ["abc","def","hjy"] (1,0)
'j'

-}

verificaNivel :: Tabuleiro -> Coordenada -> Nivel --
verificaNivel (h:t) (x,y) = posAtualX
  where
     		tt = tamTabuleiro (h:t) 

     		posAtualY = if validaPosTab ((x,y,'N'),(tt)) == True
     			          then last ( take ((length (h:t))-y) (h:t) )
     			          else "~"

     		posAtualX =	if posAtualY == "~"
     			          then '~'
     			          else if take 1(drop (x) posAtualY) == ""
     			         	     then '~'
     			         	     else head (take 1(drop (x) posAtualY))

{-| A função 'rodaDireita' rodar a orientação do Robot mediante a orientação atual do mesmo.

>>> rodaDireita N
E

>>> rodaDireita S
O

-}

rodaDireita :: Orientacao -> Orientacao
rodaDireita N = E
rodaDireita E = S
rodaDireita S = O
rodaDireita O = N

{-| A função 'rodaEsquerda' rodar a orientação do Robot mediante a orientação atual do mesmo.

>>> rodaEsquerda N
O

>>> rodaEsquerda S
E

-}

rodaEsquerda :: Orientacao -> Orientacao
rodaEsquerda N = O
rodaEsquerda E = N
rodaEsquerda S = E
rodaEsquerda O = S

{-| A função 'execComando' serve para executar o comando. Recebe o Tabuleiro, a PosicaoTabuleiro e os Comandos. 
Vai devolver a PosicaoRobot do Robot após a execução desse mesmo comando ou lista de comandos. No caso desse comando não ser possivel ou válido, este vai devolver 
a PosicaoRobot = (-1,-1,Char)

>>> execComando ["aBBC","aaAD","abCC","abDE"] ((3,0,'N'),(4,4)) ["S"]
(3,1,'N')

>>> execComando ["aBBC","aaAD","abCC","abDE"] ((3,2,'N'),(4,4)) ["A"]
(-1,-1,'N')

-}

execComando :: Tabuleiro -> PosicaoTabuleiro -> Char -> PosicaoRobot
execComando (a:b) ((x,y,o),(xx,yy)) c = resultado
 where
    nivel     = verificaNivel (a:b) (x,y)

    resultado = if validaPosTab ((x,y,o),(xx,yy)) == True
                then if c == 'D' 
                     then if o == 'N'
                          then (x,y,'E')
                          else if o == 'E'
                               then (x,y,'S')
                               else if o == 'S'
                                    then (x,y,'O')
                                    else if o == 'O'
                                         then (x,y,'N')
                                         else (-1,-1,o)
                     else if c == 'E' 
                          then if o == 'N'
                               then (x,y,'O')
                               else if o == 'O'
                                    then (x,y,'S')
                                    else if o == 'S'
                                         then (x,y,'E')
                                         else if o == 'E'
                                              then (x,y,'N')
                                              else (-1,-1,o)
                          else if c == 'S' 
                               then if o == 'N'
                                    then if ( ord (toUpper nivel) - ord (toUpper( verificaNivel (a:b) (x,y+1) )) >= 1 ) && ( ord (toUpper nivel) - ord (toUpper( verificaNivel (a:b) (x,y+1) )) <= 25 )  || (ord (toUpper nivel) - ord (toUpper( verificaNivel (a:b) (x,y+1) )) == -1 )                                    
                                         then (x,y+1,o)
                                         else (-1,-1,o)
                                    else if o == 'S'
                                         then if ( ord (toUpper nivel) - ord (toUpper( verificaNivel (a:b) (x,y-1) )) >= 1 ) && ( ord (toUpper nivel) - ord (toUpper( verificaNivel (a:b) (x,y-1) )) <= 25 )  || (ord (toUpper nivel) - ord (toUpper( verificaNivel (a:b) (x,y-1) )) == -1 )                                    
                                              then (x,y-1,o)
                                              else (-1,-1,o)
                                         else if o == 'E'
                                              then if ( ord (toUpper nivel) - ord (toUpper( verificaNivel (a:b) (x+1,y) )) >= 1 ) && ( ord (toUpper nivel) - ord (toUpper( verificaNivel (a:b) (x+1,y) )) <= 25 )  || (ord (toUpper nivel) - ord (toUpper( verificaNivel (a:b) (x+1,y) )) == -1 )                                    
                                                   then (x+1,y,o)
                                                   else (-1,-1,o)
                                              else if o == 'O'
                                                   then if ( ord (toUpper nivel) - ord (toUpper( verificaNivel (a:b) (x-1,y) )) >= 1 ) && ( ord (toUpper nivel) - ord (toUpper( verificaNivel (a:b) (x-1,y) )) <= 25 )  || (ord (toUpper nivel) - ord (toUpper( verificaNivel (a:b) (x-1,y) )) == -1 )                                    
                                                        then (x-1,y,o)
                                                        else (-1,-1,o)
                                                   else (-1,-1,o)
                               else if c == 'A' 
                                    then if o == 'N' && ( (ord (toUpper nivel) - (ord (toUpper(verificaNivel (a:b) (x,y+1)))) == 0 ))
                                         then (x,y+1,o)
                                         else if o == 'O' && ( (ord (toUpper nivel) - (ord (toUpper(verificaNivel (a:b) (x-1,y)))) == 0 ))
                                              then (x-1,y,o)
                                              else if o == 'S' && ( (ord (toUpper nivel) - (ord (toUpper(verificaNivel (a:b) (x,y-1)))) == 0 ))
                                                   then (x,y-1,o)
                                                   else if o == 'E' && ( (ord (toUpper nivel) - (ord (toUpper(verificaNivel (a:b) (x+1,y)))) == 0 ))
                                                        then (x+1,y,o)
                                                        else (-1,-1,o)
                                    else if c == 'L'
                                         then if  isUpper(verificaNivel (a:b) (x,y)) == True
                                              then (x,y,o)
                                              else (-1,-1,o)
                                         else (-1,-1,o)  
                else (-1,-1,o)

{-| A função 'outStr' recebe uma [String] e devolve uma String. Esta função
serve para apresentar o resultado da função 'tarefaA', 'tarefaB', 'tarefaC', entre outras aplicações. 

>>> outStr ["123"]
"123"

>>> outStr ["OK"]
"OK"

-}

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

----------------------------------------------------------------------------
-- TESTES LOCAIS
----------------------------------------------------------------------------

{-| A função 'testaB' é usada para testar localmente a verificação da tarefa B. Recebe uma [String] com os numeros dos tabuleiros que vão ser 
testados (sem a sua extensão que é por definição .in). Vai o resultado esperado ( lido de um ficheiro com extensão .out), e o resultado da tarefaB. Se ambos coincidirem, 
este imprime __Resultado tarefa B ------>__, se não, imprime __FALHOU NA TAREFA B _-_-_-> __.

>>> testaB ["00"]
Teste a verificar: 00
Resultado esperado-----> ERRO
Resultado tarefa ------> ERRO

>>> testaB ["00","01"]
Teste a verificar: 00
Resultado esperado-----> ERRO
Resultado tarefa ------> ERRO
Teste a verificar: 01
Resultado esperado-----> 0 0 N
Resultado tarefa ------> 0 0 N

-}

testaB :: [String] -> IO ()
testaB []    = putStrLn "\nAcabou!!!!!!!!!!!!!!!!!\n"
testaB (h:t) = do inp <- readFile ("../tests/tarefaB/teste" ++ h ++".in")
                  resultin <- readFile("../tests/tarefaB/teste" ++ h ++".out")
                  result <-return (outStr(tarefaB (lines inp)))
                  conteudoStr <- return ((lines inp) :: [String])
                  putStrLn ("Teste a verificar: " ++ h)
                  if (result == resultin)
                  then putStrLn (("Resultado esperado ------> "++resultin) ++ "Resultado tarefa B ------> " ++ result)
                  else putStrLn (("RESULTADO ESPERADO _-_-_-> "++resultin) ++ "FALHOU NA TAREFA B _-_-_-> " ++ result)
                  testaB t

{-| A função 'b' contem apenas uma listagem de testes da tarefa B.

>>> b = ["00", "01", "02", "03", "04", "05", "06", "07", "08", "09","10", "11", "12", "13", "14", "15", "16", "17", "18", "19","20", "21", "22", "23", "24"]

-}

-- Testes tarefa B
b :: [String]
b = ["00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
     "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
     "20", "21", "22", "23", "24"]