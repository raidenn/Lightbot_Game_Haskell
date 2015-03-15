----------------------------------------------------------------------------
--                               LIGHTBOT
----------------------------------------------------------------------------

{-| __TAREFA C__

O programa deve executar a sequência de comandos no robot. Deve atender aos seguintes aspectos:

    - assume-se que o formato do ficheiro de entrada é válido;
    - os comandos são executados em sequência;
    - comandos que não sejam aplicáveis deverão deixar o estado do /robot/ inalterado (tal como na versão do jogo online);
    - sempre que um comando __L__ for executado com sucesso deverá ser impressa uma linha contendo as cordenadas x e y da posição
      onde o robot se encontra (separadas por um único espaço);
    - quando todas a lâmpadas do tabuleiro se encontrarem ligadas, o programa imprime uma linha com a mensagem __FIM (tick_count)__ 
      (em que (tick_count) é o número de comandos válidos executados);
    - se a sequência de comandos terminar sem que todas as lâmpadas se encontrem ligadas deve imprimir a mensagem INCOMPLETO
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
    putStr (outStr (tarefaC (lines inp)))

----------------------------------------------------------------------------
-- TAREFA C
----------------------------------------------------------------------------

{-| A função 'tarefaC' serve para executar todos os comandos fornecidos em sequencia num determinado tabuleiro. Recebe uma [String], correspondete ao Input fornecido e 
vai devolver uma [String] com todas as coordenadas onde o comando __L__ é executado com sucesso e uma mensagem final que pode ser __FIM (tick_count)__ (onde tick count é o 
numero de comandos válidos executados), quando todos as lampadasdo tabuleiro ficam acesas. __INCOMPLETO__ no caso de acabar a lista de comandos e nem todas as lampadas do tabuleiro 
estarem acesas. 

>>>  tarefaC ["AbB","AAa","0 0 E","LALAESLEASL"]
["0 0","1 0","2 1","0 1","FIM 11"]

>>> tarefaC ["AbB","AAa","0 0 E","LALAESL"] 
["0 0","1 0","2 1","INCOMPLETO"]

-}

tarefaC :: [String] -> [String]
tarefaC l = result
 where 
       tabuleiro   = takeWhile validaLetras l -- Guarda na variavel 'tabuleiro' apenas as linhas correspondentes ao tabuleiro de jogo. takeWhile vai ler a '[String]'' e vai manter os elementos enquando apensas existerem letras nesse elemento.
       coordenadas = take 1 (dropWhile validaLetras l) -- Guarda na variavel 'coordenadas' apenas a linha correspondente às coordenadas iniciais do robot. Usamos o 'dropWhile' para deitar fora os elementos correspondentes às linhas do tabuleiro e manter as restantes. Fazemos o 'take 1' pois sabemos que apenas a 1ª irá conter as coordenadas.
       comandos    = head(drop 1 (dropWhile validaLetras l)) --Guarda na variavel 'comandos' apenas a linha correspondente aos comandos a ser executados pelo robot. Usamos também o 'dropWhile' mas agora fazemos um 'drop 1' para deitar fora a linha das coordenadas e ficar com o restante.
       
       tamTab      = tamTabuleiro tabuleiro -- Na variavel 'tamanhoY' guardamos o tamanho correspondente ao eixo yy.
       pInicial    = posInicial coordenadas -- Na variavel 'tamanhoX' guardamos o tamanho correspondente ao eixo xx.
       
       contaLamp   = contaLampadas tabuleiro -- Conta o numero de lampadas num tabuleiro
       
       result      = correJogo contaLamp tabuleiro (pInicial,tamTab) comandos

{-| A função 'correJogo' é a função __main__ da Tarefa C. Esta recebe todos os elementos necessario para executar cada comando fornecido em sequencia. 
Resultado igual à função tarefaC, só difere nos elementos que recebe.

>>>   correJogo 0 1 0 ["abb","Aaa"] ((0,0,'N'),(3,2)) "ADDAL" [] []
["0 0","FIM 5"]

>>> correJogo 0 2 0 ["abb","AaA"] ((0,0,'N'),(3,2)) "ADDAL" [] []
["0 0","INCOMPLETO"]

-}

correJogo ::  LampadasTabuleiro -> Tabuleiro -> PosicaoTabuleiro -> Comandos -> [String]
correJogo k (a:b) ((p,j,m),(xx,yy)) (c:d) = aux 0 0 (p,j,m) (c:d) [] []
  where aux n com (p,j,m) [] l q = if n < k
                                   then l ++ ["INCOMPLETO"]
                                   else l ++ ["FIM" ++ " " ++ (show com)]
       
        aux n com (p,j,m) (y:ys) l q = if n < k
                                      then if y == 'L'
                                           then if execComando (a:b) ((p,j,m),(xx,yy)) y == (-1,-1,m)
                                                then aux n com (p,j,m) ys l q
                                                else if elem (imprimePosLamp (a:b) ((p,j,m),(xx,yy)) y) q == True
                                                     then aux (n-1) (com + 1) (p,j,m) ys (l ++ [(imprimePosLamp (a:b) ((p,j,m),(xx,yy)) y)]) (filter (/=imprimePosLamp (a:b) ((p,j,m),(xx,yy)) y) q)
                                                     else aux (n+1) (com + 1) (p,j,m) ys (l ++ [(imprimePosLamp (a:b) ((p,j,m),(xx,yy)) y)]) (q ++ [(imprimePosLamp (a:b) ((p,j,m),(xx,yy)) y)])
                                           else if execComando (a:b) ((p,j,m),(xx,yy)) y == (-1,-1,m)
                                                then aux n com (p,j,m) ys l q
                                                else aux n (com + 1)  (execComando (a:b) ((p,j,m),(xx,yy)) y) ys l q
                                      else l ++ ["FIM" ++ " " ++ (show com)]

{-| A função 'verificaLampadas' verifica o numero de lampadas que existem numa linha do tabuleiro.

>>>   verificaLampadas "abAbbA"
2

>>> verificaLampadas "AbAbbA"
3

>>> verificaLampadas "ababba"
0

-}

verificaLampadas :: LinhaTabuleiro -> Int
verificaLampadas (h:t) = aux 0 (h:t)
  where aux n []     = n
        aux n (y:ys) = if isUpper y == True then aux (n+1) ys
                       else aux n ys

{-| A função 'contaLampadas' verifica o numero de lampadas que existem numa linha do tabuleiro.

>>> contaLampadas ["abAbbA"]
2

>>> contaLampadas ["abAbbA","aAAa"]
4

-}

contaLampadas :: Tabuleiro -> Int
contaLampadas (h:t) = aux 0 (h:t)
  where aux n []     = n
        aux n (y:ys) = if verificaLampadas y > 0 then aux (n+(verificaLampadas y)) ys
                       else aux n ys


{-| A função 'acendeLampada' verifica se determinada coordenada contem uma lampada e devolve o valor __True__ ou __False__. Para tal, recebe um Tabuleiro e uma PosicaoTabuleiro.

>>> acendeLampada ["abAb","aAAa"] ((1,1,'N'),(4,2))
False

>>> aacendeLampada ["abAb","AAAa"] ((0,0,'N'),(4,2))
True

-}

acendeLampada :: Tabuleiro -> PosicaoTabuleiro -> Bool 
acendeLampada (a:b) ((x,y,o),(xx,yy)) = if  isUpper(verificaNivel (a:b) (x,y)) == True
								                        then True 
								                        else False

{-| A função 'imprimePosLamp' devolve a coordenada x y , sob forma de string no caso de o camndo __L__ ser utilizado numa coordenada que contem uma lamapda ou não. 
Se tiver lampada, devolve a coordenada, se não, devolve __"~"__.

>>> imprimePosLamp  ["abAb","AAAa"] ((0,0,'N'),(4,2)) 'L'
"0 0"

>>> imprimePosLamp  ["abAb","aAAa"] ((0,0,'N'),(4,2)) 'L'
"~"

-}

imprimePosLamp :: Tabuleiro -> PosicaoTabuleiro -> Char -> String
imprimePosLamp (a:b) ((x,y,o),(xx,yy)) f =  if f == 'L'
				       	                            then if execComando (a:b) ((x,y,o),(xx,yy)) f == (-1,-1,o)
				                                         then "~"
				                                         else (show x ++ " " ++ show y)
				                                    else "~"

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

        posAtualX = if posAtualY == "~"
                    then '~'
                    else if take 1(drop (x) posAtualY) == ""
                         then '~'
                         else head (take 1(drop (x) posAtualY))

{-| A função 'posInicial' serve para verificar a posição inicial do robot. Recebe uma [String] com as cordenadas e vai devolver essas mesmas coordenadas no 
formato (Int,Int,Char)

>>> posInicial ["1 1 N"]
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

tamTabuleiro :: Tabuleiro -> TamanhoTabuleiro
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

{-| A função 'validaLetras' vai verificar se os elementos da String são letras do alfabeto, sejas estas maiúsculas ou minúsculas.
 Devolve __True__ ou __False__ respetivamente.

>>>validaLetras "aacdD"
True

>>> validaLetras "5bCaa"
False

-}                             

validaLetras :: String -> Bool
validaLetras str = and (map isLetter str)

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

{-| A função 'testaC' é usada para testar localmente a verificação da tarefa C. Recebe uma [String] com os numeros dos tabuleiros que vão ser 
testados (sem a sua extensão que é por definição .in). Vai o resultado esperado ( lido de um ficheiro com extensão .out), e o resultado da tarefa C. Se ambos coincidirem, 
este imprime __Resultado tarefa C ------>__, se não, imprime __FALHOU NA TAREFA C _-_-_-> __.

>>> testaC ["teste00"]
Teste a verificar: 00
Resultado esperado
2 0
4 2
FIM 12
Resultado tarefa C
2 0
4 2
FIM 12


>>> testaC ["00","01"]
Teste a verificar: 00
Resultado esperado
2 0
4 2
FIM 12
Resultado tarefa C
2 0
4 2
FIM 12
Teste a verificar: 01
Resultado esperado
17 17
17 16
17 15
17 0
FIM 21
Resultado tarefa C
17 17
17 16
17 15
17 0
FIM 21

-}

testaC :: [String] -> IO ()
testaC [] = putStrLn "\nAcabou!!!!!!!!!!!!!!!!!\n"
testaC (h:t) = do inp <- readFile ("../tests/tarefaC/teste" ++ h ++".in")
                  resultin <- readFile("../tests/tarefaC/teste" ++ h ++".out")
                  result <-return (outStr(tarefaC (lines inp)))
                  conteudoStr <- return ((lines inp) :: [String])
                  putStrLn ("Teste a verificar: " ++ h)
                  if (result == resultin)
                  then putStrLn (("Resultado esperado\n"++resultin) ++ "Resultado tarefa C\n" ++ result)
                  else putStrLn (("RESULTADO ESPERADO\n"++resultin) ++ "FALHOU NA TAREFA C\n" ++ result)
                  testaC t

{-| A função 'c' contem apenas uma listagem de testes da tarefa C.

>>> c = ["00", "01", "02", "03", "04", "05", "06", "07", "08", "09","10", "11", "12", "13", "14", "15", "16", "17", "18", "19","20", "21", "22", "23", "24", "25", "50","51","52","53"]

-}

-- Testes tarefa C
c :: [String]
c = ["00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
     "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
     "20", "21", "22", "23", "24", "25", "50","51","52","53"]