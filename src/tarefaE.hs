----------------------------------------------------------------------------
--                               L I G H T B O T
----------------------------------------------------------------------------

{-| __TAREFA E__

Deve realizar um programa que permita visualizar o jogo Lightbot com recurso ao formato X3dom (http://www.x3dom.org). 

Como requisito mínimo, a visualização deve mostrar o tabuleiro, a posição inicial e das lâmpadas. Como valorização, sugere-se a animação da execução da sequência de comandos para o robot.

O input é fornecido seguindo o formato de entrada adoptado na primeira fase do projecto. 

Como output, o programa deve imprimir o código html de uma página web que permita visualizar o jogo LightBot.

-}

module Main where

import Data.Char
import Data.List

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
type PosicaoLampadas = [Coordenada]
type Direcao = (Int,Int)
type Grafo = (Lvl,Coordenada)
type Lvl = Int

----------------------------------------------------------------------------
-- IO
----------------------------------------------------------------------------

main = do  

    inp <- getContents
    tarefaE (lines inp)

----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- TAREFA E
----------------------------------------------------------------------------

{-| A função 'tarefaE' recebe uma lista de String com as linhas do tabuleiro, a posicao inicial do robot e pode ou não também receber uma linha de comandos para ser executada.
 Esta vai devolver o codigo <html> correspondente à animação desse jogo.

-}

tarefaE :: [String] -> IO ()
tarefaE l = if givenCom == []
                   then criaX3domFile tabuleiro (pInicial,tamTab) comandos (comandGerLid givenCom)
                   else criaX3domFile tabuleiro (pInicial,tamTab) (head(givenCom)) (comandGerLid givenCom)
  where
               tabuleiro    = takeWhile validaLetras l 
               coordenadas  = take 1 (dropWhile validaLetras l)
               givenCom     = drop 1 (dropWhile validaLetras l)
               pInicial     = posInicial coordenadas
               
               tamTab       = tamTabuleiro tabuleiro
               
               posiLamp     = posicaoLampadas tabuleiro tamTab

               comandos     = resolveTabul tabuleiro (pInicial,tamTab) posiLamp
               

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

{-| A função 'posicaoLampadas' serve para verificar o tabuleiro e devolver as coordenadas das posições onde se encontram lâmpadas. 
Esta recebe o tabuleiro e o seu tamanho. Devolve uma lista de coordenadas.

>>>  posicaoLampadas ["aaA","aAa","AaA"] (3,3) 
[(2,2),(1,1),(0,0),(2,0)]

>>> posicaoLampadas ["aaa","aaa","AaA"] (3,3) 
[(0,0),(2,0)]

-}

posicaoLampadas :: Tabuleiro -> TamanhoTabuleiro -> PosicaoLampadas
posicaoLampadas l (x,y) = aux l (0,(y-1)) []
    where
          aux []         (k,y) result = result
          aux ([]:ta)    (k,y) result = aux ta (0,(y-1)) result
          aux ((h:t):ta) (k,y) result | isUpper h == True = aux (t:ta) (k+1,y) (result ++ [(k,y)])
                                      | otherwise         = aux (t:ta) ((k+1),y) result

{-| A função 'proximaLampadaProxima' serve para determinar, numa posição, quais as lâmpadas mais próximas. 
Esta recebe a posição do Robot e a posição de todas as lâmpadas do tabuleiro. Vai devolver uma lista com as direções , ordenadas por proximidade, para todas as lampadas. 

>>>  proximaLampadaProxima (0,0,'N') [(2,3),(0,2),(4,0)]
[(0,2),(4,0),(2,3)]

>>> proximaLampadaProxima (2,2,'N') [(2,3),(0,2),(4,0)]
[(0,1),(-2,0),(2,-2)]

-}

proximaLampadaProxima :: PosicaoRobot -> PosicaoLampadas -> [Direcao]
proximaLampadaProxima posR posL = sortDir (aux posR posL [])
    where
          aux (x,y,o) []          result  = result
          aux (x,y,o) ((xx,yy):t) result  | (xx-x,yy-y) /= (0,0) = aux (x,y,o) t (result ++ [((xx-x),(yy-y))])
                                          | otherwise            = aux (x,y,o) t (result ++ [(0,0)])

{-| A função 'distLamp' calcula a soma dos elementos de uma vetor.

>>>  distLamp (2,3)
5

>>> distLamp (1,2)
3

-}

distLamp:: Direcao -> Int
distLamp(x,y) = abs(x)+abs(y)

{-| A função 'sortDir' ordena por numero de movimentos necessários uma lista de vetores/direções.

>>>  sortDir [(2,3),(1,2)]
[(1,2),(2,3)]

-}

sortDir:: [Direcao] -> [Direcao]
sortDir list = reverse(sortBy compCoord list)

{-| A função 'compCoord' é função auxiliar usada na função sortDir. Esta lê dois vetores e ordena por numero de movimentos necessários.

>>>  compCoord (2,3) (1,2)
LT

-}

compCoord:: Direcao -> Direcao -> Ordering
compCoord (a,b) (c,d) 
        | distLamp(a,b) >= distLamp(c,d) = LT
        | distLamp(a,b) <= distLamp(c,d) = GT

{-| A função 'gerarComandosSimples' resolve um tabuleiro simples. Este assume que o robot pode sempre avançar ou saltar para qualquer posição válida do tabuleiro. 
Se esta condição não se verificar apresenta a mensagem "ERRO". Recebe um tabuleiro, a posição inicial do robot e a posição das lampadas do tabuleiro.

>>>  gerarComandosSimples ["aaa","bBb","aaa"] ((0,0,'N'),(3,3)) [(1,1)]
"SDAL"

>>>  gerarComandosSimples ["Aaa","bBb","Aaa"] ((0,0,'N'),(3,3)) [(1,1),(0,0),(0,2)]
"LSSLDADSL"

-}

gerarComandosSimples :: Tabuleiro -> PosicaoTabuleiro -> PosicaoLampadas -> Comandos
gerarComandosSimples tab ((x,y,o),(xx,yy)) posL = aux (x,y,o) posL (proximaLampadaProxima (x,y,o) posL) []
    where
            aux (x,y,o) []    []          result = result
            aux (x,y,o) posL  ((xx,yy):t) result = if elem (x,y) posL == True
                                                   then aux (x,y,o) (filter (/= (x,y)) posL) (proximaLampadaProxima (x,y,o) (filter (/= (x,y)) posL)) (result ++ "L")
                                                   else if yy > 0
                                                        then if o /= 'N'
                                                             then if o == 'O' || o == 'S'
                                                                  then aux (x,y,(rodaDireita o)) posL  ((xx,yy):t) (result ++ "D")
                                                                  else aux (x,y,(rodaEsquerda o)) posL  ((xx,yy):t) (result ++ "E")
                                                             else if ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x,y+1))) == 0
                                                                  then aux ((x,y+1,o)) posL (proximaLampadaProxima (x,y+1,o) posL) (result ++ "A")
                                                                  else if (ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x,y+1))) >= 1 && ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x,y+1))) <= 25) || ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x,y+1))) == -1
                                                                       then aux ((x,y+1,o)) posL (proximaLampadaProxima (x,y+1,o) posL) (result ++ "S")
                                                                       else "ERROR"
                                                        else if xx > 0
                                                             then if o /= 'E'
                                                                  then if o == 'N' || o == 'O'
                                                                       then aux (x,y,(rodaDireita o)) posL  ((xx,yy):t) (result ++ "D")
                                                                       else aux (x,y,(rodaEsquerda o)) posL  ((xx,yy):t) (result ++ "E")
                                                                  else if ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x+1,y))) == 0
                                                                       then aux ((x+1,y,o)) posL (proximaLampadaProxima ((x+1),y,o) posL) (result ++ "A")
                                                                       else if (ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x+1,y))) >= 1 && ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x+1,y))) <= 25) || ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x+1,y))) == -1
                                                                            then aux ((x+1,y,o)) posL (proximaLampadaProxima ((x+1),y,o) posL) (result ++ "S")
                                                                            else "ERROR"
                                                             else if yy < 0
                                                                  then if o /= 'S'
                                                                       then if o == 'N' || o == 'E'
                                                                            then aux (x,y,(rodaDireita o)) posL  ((xx,yy):t) (result ++ "D")
                                                                            else aux (x,y,(rodaEsquerda o)) posL  ((xx,yy):t) (result ++ "E")
                                                                       else if ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x,y-1))) == 0
                                                                            then aux ((x,y-1,o)) posL (proximaLampadaProxima (x,y-1,o) posL) (result ++ "A")
                                                                            else if (ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x,y-1))) >= 1 && ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x,y-1))) <=25) || ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x,y-1))) == -1
                                                                                 then aux ((x,y-1,o)) posL (proximaLampadaProxima (x,y-1,o) posL) (result ++ "S")
                                                                                 else "ERROR"
                                                                  else if xx < 0
                                                                       then if o /= 'O'
                                                                            then if o == 'S' || o == 'E'
                                                                                 then aux (x,y,(rodaDireita o)) posL  ((xx,yy):t) (result ++ "D")
                                                                                 else aux (x,y,(rodaEsquerda o)) posL  ((xx,yy):t) (result ++ "E")
                                                                            else if ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x-1,y))) == 0
                                                                                 then aux ((x-1,y,o)) posL (proximaLampadaProxima ((x-1),y,o) posL) (result ++ "A")
                                                                                 else if (ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x-1,y))) >= 1 && ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x-1,y))) <= 25) || ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x-1,y))) == -1
                                                                                      then aux ((x-1,y,o)) posL (proximaLampadaProxima ((x-1),y,o) posL) (result ++ "S")
                                                                                      else "ERROR"
                                                                       else "ERROR"

{-| A função 'verificaPossiveisMov' lê um tabuleiro e uma posição e verifica quais as coordenadas para onde o robot se pode movimentar. 

>>>  verificaPossiveisMov  ["Aaa","bBb","Aaa"] ((0,0,'N'),(3,3))
[(0,1),(1,0)]

>>>  verificaPossiveisMov  ["Aaa","bBb","Aaa"] ((1,2,'N'),(3,3))
[(1,1),(2,2),(0,2)]

-}

verificaPossiveisMov :: Tabuleiro -> PosicaoTabuleiro -> [Coordenada]
verificaPossiveisMov tab ((x,y,o),(xx,yy)) = r1 ++ r2 ++ r3 ++ r4 ++ r5 ++ r6 ++ r7 ++ r8
    where
            r1 =    if execComando tab ((x,y,'N'),(xx,yy)) 'A' /= (-1,-1,'N') then [(x,y+1)] else []
            r2 =    if execComando tab ((x,y,'N'),(xx,yy)) 'S' /= (-1,-1,'N') then [(x,y+1)] else []
            r3 =    if execComando tab ((x,y,'S'),(xx,yy)) 'A' /= (-1,-1,'S') then [(x,y-1)] else []
            r4 =    if execComando tab ((x,y,'S'),(xx,yy)) 'S' /= (-1,-1,'S') then [(x,y-1)] else []
            r5 =    if execComando tab ((x,y,'E'),(xx,yy)) 'A' /= (-1,-1,'E') then [(x+1,y)] else []
            r6 =    if execComando tab ((x,y,'E'),(xx,yy)) 'S' /= (-1,-1,'E') then [(x+1,y)] else []
            r7 =    if execComando tab ((x,y,'O'),(xx,yy)) 'A' /= (-1,-1,'O') then [(x-1,y)] else []
            r8 =    if execComando tab ((x,y,'O'),(xx,yy)) 'S' /= (-1,-1,'O') then [(x-1,y)] else []
                                     
{-| A função 'getCommands' devolve a lista de comandos até uma determinada lâmpada, assumindo que nem sempre o robot se pode movimentar para qualquer posição válida do tabuleiro. 
Esta função verifica se existe alguma lampada num sitio onde , após chegar à sua posição, não seja mais possivel movimentar-se. Nesse caso devolve apenas os comandos 
até à posição anterior a essa lâmpada.

>>>  getCommands  ["zzz","zAz","zzz"] ((0,0,'N'),(3,3)) [(1,1)]  (1,1)
"AADAD"

>>>  getCommands  ["zzZ","zaz","zzz"] ((0,0,'N'),(3,3)) [(2,2)]  (2,2)
"AADAAL"

-}

getCommands :: Tabuleiro -> PosicaoTabuleiro -> PosicaoLampadas -> Direcao -> Comandos
getCommands tab ((x,y,o),(xx,yy)) posL (q,w) = aux posL (x,y,o) [] [] (verificaPossiveisMov tab ((x,y,o),(xx,yy))) []
    where
            
            posLamp = (x+q,y+w)
            
            aux posL (z,v,u) camPerc camSSaida  []        result | (z,v) == posLamp && (verificaPossiveisMov tab ((z,v,u),(xx,yy))) == [] = (take (length (result)-1) result)
                                                                 | (z,v) == posLamp = result ++ "L"
                                                                 | otherwise        = aux posL (x,y,o) [] (camSSaida ++ [(z,v)])  (verificaPossiveisMov tab ((x,y,o),(xx,yy))) []
            
            aux posL (z,v,u) camPerc camSSaida  ((g,h):t) result =   if (z,v) == (posLamp)
                                                                        then result ++ "L"
                                                                        else if elem (z,v) posL == True
                                                                             then aux (filter (/= (z,v)) posL) (z,v,u) camPerc camSSaida ((g,h):t) (result ++ "L")
                                                                             else if elem (g,h) camSSaida == True || elem (g,h) camPerc == True
                                                                                     then aux posL (z,v,u) camPerc camSSaida t result
                                                                                     else if g-z == 1
                                                                                          then aux posL (z+1,v,'E') (camPerc ++ [(z,v)]) camSSaida   (filter (\(x,y) -> (x,y) /= (z,v)) (verificaPossiveisMov tab ((z+1,v,u),(xx,yy)))) (result ++ (printDirections tab (z,v,u) [(1,0)]))
                                                                                          else if h-v == 1
                                                                                               then aux posL (z,v+1,'N') (camPerc ++ [(z,v)]) camSSaida   (filter (\(x,y) -> (x,y) /= (z,v)) (verificaPossiveisMov tab ((z,v+1,u),(xx,yy)))) (result ++ (printDirections tab (z,v,u) [(0,1)]))
                                                                                               else if g-z == -1
                                                                                                    then aux posL (z-1,v,'O') (camPerc ++ [(z,v)]) camSSaida   (filter (\(x,y) -> (x,y) /= (z,v)) (verificaPossiveisMov tab ((z-1,v,u),(xx,yy)))) (result ++ (printDirections tab (z,v,u) [(-1,0)]))
                                                                                                    else if h-v == -1
                                                                                                         then aux posL (z,v-1,'S') (camPerc ++ [(z,v)]) camSSaida   (filter (\(x,y) -> (x,y) /= (z,v)) (verificaPossiveisMov tab ((z,v-1,u),(xx,yy)))) (result ++ (printDirections tab (z,v,u) [(0,-1)]))
                                                                                                         else result

{-| A função 'getAllCommands' devolve a lista de comandos até uma determinada lâmpada, assumindo que nem sempre o robot se pode movimentar para qualquer posição válida do tabuleiro. 

>>>  getAllCommands  ["zzz","zAz","zzz"] ((0,0,'N'),(3,3)) [(1,1)]  (1,1)
"AADADSL"

-}

getAllCommands :: Tabuleiro -> PosicaoTabuleiro -> PosicaoLampadas -> Direcao -> Comandos
getAllCommands tab ((x,y,o),(xx,yy)) posL (q,w) = aux posL (x,y,o) [] [] (verificaPossiveisMov tab ((x,y,o),(xx,yy))) []
    where
            
            posLamp = (x+q,y+w)
            
            aux posL (z,v,u) camPerc camSSaida  []        result | (z,v) == posLamp = result ++ "L"
                                                                 | otherwise        = aux posL (x,y,o) [] (camSSaida ++ [(z,v)])  (verificaPossiveisMov tab ((x,y,o),(xx,yy))) []
            
            aux posL (z,v,u) camPerc camSSaida  ((g,h):t) result =   if (z,v) == (posLamp)
                                                                        then result ++ "L"
                                                                        else if elem (z,v) posL == True
                                                                             then aux (filter (/= (z,v)) posL) (z,v,u) camPerc camSSaida ((g,h):t) (result ++ "L")
                                                                             else if elem (g,h) camSSaida == True || elem (g,h) camPerc == True
                                                                                     then aux posL (z,v,u) camPerc camSSaida t result
                                                                                     else if g-z == 1
                                                                                          then aux posL (z+1,v,'E') (camPerc ++ [(z,v)]) camSSaida   (filter (\(x,y) -> (x,y) /= (z,v)) (verificaPossiveisMov tab ((z+1,v,u),(xx,yy)))) (result ++ (printDirections tab (z,v,u) [(1,0)]))
                                                                                          else if h-v == 1
                                                                                               then aux posL (z,v+1,'N') (camPerc ++ [(z,v)]) camSSaida   (filter (\(x,y) -> (x,y) /= (z,v)) (verificaPossiveisMov tab ((z,v+1,u),(xx,yy)))) (result ++ (printDirections tab (z,v,u) [(0,1)]))
                                                                                               else if g-z == -1
                                                                                                    then aux posL (z-1,v,'O') (camPerc ++ [(z,v)]) camSSaida   (filter (\(x,y) -> (x,y) /= (z,v)) (verificaPossiveisMov tab ((z-1,v,u),(xx,yy)))) (result ++ (printDirections tab (z,v,u) [(-1,0)]))
                                                                                                    else if h-v == -1
                                                                                                         then aux posL (z,v-1,'S') (camPerc ++ [(z,v)]) camSSaida   (filter (\(x,y) -> (x,y) /= (z,v)) (verificaPossiveisMov tab ((z,v-1,u),(xx,yy)))) (result ++ (printDirections tab (z,v,u) [(0,-1)]))
                                                                                                         else result



{-| A função 'resolveTabul' resolve um tabuleiro. Devolve todos os comandos para apagar todas as lampadas e assim terminar o jogo.

>>>  resolveTabul   ["zca","bDb","acC"] ((2,2,'S'),(3,3)) [(1,1),(2,0)]
"SSLDADSL"

>>>  resolveTabul   ["Fea","bDb","acC"] ((2,2,'S'),(3,3)) [(0,2),(1,1),(2,0)]
"SSLDADSLSESL"

-}

resolveTabul :: Tabuleiro -> PosicaoTabuleiro -> PosicaoLampadas -> Comandos
resolveTabul tab ((x,y,o),(xx,yy)) []   = ""
resolveTabul tab ((x,y,o),(xx,yy)) posL = if contaL (gerarComandosSimples tab ((x,y,o),(xx,yy)) posL) == length (posicaoLampadas tab (xx,yy))
                                          then gerarComandosSimples tab ((x,y,o),(xx,yy)) posL
                                          else aux [] posL (x,y,o) (proximaLampadaProxima (x,y,o) posL) []
    where 
            
            aux []          []   (x,y,o) []        result   = result
            
            aux ((z,v,u):t) []   (x,y,o) []        result   = result ++ (getAllCommands tab ((x,y,o),(xx,yy)) [(z,v)] (z-x,v-y)  )
            
            aux hole        posL (x,y,o) ((l,p):t) result = if (getCommands tab ((x,y,o),(xx,yy)) posL (l,p)) /= (getAllCommands tab ((x,y,o),(xx,yy)) posL (l,p))
                                                            
                                                            then aux (hole ++ [(getPosition tab ((x,y,o),(xx,yy)) (getAllCommands tab ((x,y,o),(xx,yy)) posL (l,p))) ])                (getLampsON tab ((x,y,o),(xx,yy)) posL (getAllCommands tab ((x,y,o),(xx,yy)) posL (l,p)))                 (getPosition tab ((x,y,o),(xx,yy)) (getCommands tab ((x,y,o),(xx,yy)) posL (l,p)))                  ( proximaLampadaProxima (getPosition tab ((x,y,o),(xx,yy)) (getCommands tab ((x,y,o),(xx,yy)) posL (l,p))) (getLampsON tab ((x,y,o),(xx,yy)) posL (getAllCommands tab ((x,y,o),(xx,yy)) posL (l,p))))     (result ++ (getCommands tab ((x,y,o),(xx,yy)) posL (l,p))) 
                                                            
                                                            else aux hole (getLampsON tab ((x,y,o),(xx,yy)) posL (getCommands tab ((x,y,o),(xx,yy)) posL (l,p))) (getPosition tab ((x,y,o),(xx,yy)) (getCommands tab ((x,y,o),(xx,yy)) posL (l,p)))   ( proximaLampadaProxima (getPosition tab ((x,y,o),(xx,yy)) (getCommands tab ((x,y,o),(xx,yy)) posL (l,p))) (getLampsON tab ((x,y,o),(xx,yy)) posL (getCommands tab ((x,y,o),(xx,yy)) posL (l,p))))     (result ++ (getCommands tab ((x,y,o),(xx,yy)) posL (l,p)))  ---- filter????

{-| A função 'getLampsON' recebe um Tabuleiro, PosicaoTabuleiro, PosicaoLampadas e os Comandos e vai devolver a PosicaoLampadas após executar os comandos fornecidos.

>>>  getLampsON ["EDc","aab"] ((0,0,'E'),(3,2)) [(0,1),(1,1)] "ASESESLSL"
[]

>>>  getLampsON ["EDc","aab"] ((0,0,'E'),(3,2)) [(0,1),(1,1),(3,3)] "ASESESLSL"
[(3,3)]

-} 

getLampsON :: Tabuleiro -> PosicaoTabuleiro -> PosicaoLampadas -> Comandos -> PosicaoLampadas
getLampsON tab ((x,y,o),(xx,yy)) posL [] = posL
getLampsON tab ((x,y,o),(xx,yy)) posL (h:t) | h == 'L' = getLampsON tab ((execComando tab ((x,y,o),(xx,yy)) h),(xx,yy)) (filter (/= (x,y)) posL) t
                                            | otherwise = getLampsON tab ((execComando tab ((x,y,o),(xx,yy)) h),(xx,yy)) posL t



{-| A função 'contaL' conta o numero de caratéres 'L' numa String.

>>>  contaL "SSLDADSL"
2

>>>  contaL "SSLDADSLADLAL"
4

-}

contaL :: Comandos -> Int
contaL com = aux com 0
    where
            aux [] n = n
            aux (h:t) n | h == 'L' = aux t n+1
                        | otherwise = aux t n
                                                                                         
{-| A função 'getPosition' devolve a posição do robot após executar uma série de comandos.

>>>  getPosition   ["Aaa","bBb","Aaa"] ((0,0,'N'),(3,3)) "SSDAA"
(2,2,'E')

-}

getPosition :: Tabuleiro -> PosicaoTabuleiro -> Comandos -> PosicaoRobot
getPosition tab ((z,u,v),(xx,yy)) [] = (z,u,v)
getPosition tab ((z,u,v),(xx,yy)) (h:t) = aux (h:t) (z,u,v)
    where
            aux [] (x,y,o) = (x,y,o)
            aux (h:t) (x,y,o)  = aux t (execComando tab ((x,y,o),(xx,yy)) h)


{-| A função 'printDirections' imprime, apartir de uma lista de direções, os comandos correspondentes aquelas direçoes.

>>>  printDirections  ["Aaa","bBb","Aaa"] (0,0,'N') [(1,0)]
"DA"

>>>  printDirections  ["Aaa","bBb","Aaa"] (0,0,'N') [(0,2)]
"S"

-}

printDirections :: Tabuleiro -> PosicaoRobot -> [Direcao] -> String 
printDirections tab posR dir = aux posR dir []
    where
            aux (x,y,o) []          result = result
            aux (x,y,o) ((xx,yy):t) result =        if yy > 0
                                                    then if o /= 'N'
                                                         then if o == 'O' || o == 'S'
                                                              then aux (x,y,(rodaDireita o))  ((xx,yy):t) (result ++ "D")
                                                              else aux (x,y,(rodaEsquerda o)) ((xx,yy):t) (result ++ "E")
                                                         else if ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x,y+1))) == 0
                                                              then aux ((x,y+1,'N')) t (result ++ "A")
                                                              else aux ((x,y+1,'N')) t (result ++ "S")
                                                    else if xx > 0
                                                         then if o /= 'E'
                                                              then if o == 'N' || o == 'O'
                                                                   then aux (x,y,(rodaDireita o))  ((xx,yy):t) (result ++ "D")
                                                                   else aux (x,y,(rodaEsquerda o)) ((xx,yy):t) (result ++ "E")
                                                              else if ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x+1,y))) == 0
                                                                   then aux ((x+1,y,'E')) t (result ++ "A")
                                                                   else aux ((x+1,y,'E')) t (result ++ "S")
                                                         else if yy < 0
                                                              then if o /= 'S'
                                                                   then if o == 'N' || o == 'E'
                                                                        then aux (x,y,(rodaDireita o)) ((xx,yy):t) (result ++ "D")
                                                                        else aux (x,y,(rodaEsquerda o)) ((xx,yy):t) (result ++ "E")
                                                                   else if ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x,y-1))) == 0
                                                                        then aux ((x,y-1,'S')) t (result ++ "A")
                                                                        else aux ((x,y-1,'S')) t (result ++ "S")
                                                              else if xx < 0
                                                                   then if o /= 'O'
                                                                        then if o == 'S' || o == 'E'
                                                                             then aux (x,y,(rodaDireita o)) ((xx,yy):t) (result ++ "D")
                                                                             else aux (x,y,(rodaEsquerda o)) ((xx,yy):t) (result ++ "E")
                                                                        else if ord (toUpper (verificaNivel tab (x,y))) - ord (toUpper(verificaNivel tab (x-1,y))) == 0
                                                                             then aux ((x-1,y,'O')) t (result ++ "A")
                                                                             else aux ((x-1,y,'O')) t (result ++ "S")
                                                                   else result ++ "TABULEIRO INVALIDO"


{-| A função 'rodaDireita' rodar a orientação do Robot mediante a orientação atual do mesmo.

>>> rodaDireita N
E

>>> rodaDireita S
O

-}

rodaDireita :: Nivel -> Nivel
rodaDireita 'N' = 'E'
rodaDireita 'E' = 'S'
rodaDireita 'S' = 'O'
rodaDireita 'O' = 'N'

{-| A função 'rodaEsquerda' rodar a orientação do Robot mediante a orientação atual do mesmo.

>>> rodaEsquerda N
O

>>> rodaEsquerda S
E

-}

rodaEsquerda :: Nivel -> Nivel
rodaEsquerda 'N' = 'O'
rodaEsquerda 'E' = 'N'
rodaEsquerda 'S' = 'E'
rodaEsquerda 'O' = 'S'

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

{-| A função 'validaPosTab' serve para verificar verificar se uma determinada posição pertence a um determinado tabuleiro. 
Recebe a coordenada em questão e o tamanho do tabuleiro como forma de ((Int,Int,Char),(Int,Int)), respetivamente. Devolve __True__ ou __False__.

>>> validaPosTab ((1,1,'N'),(3,3))
True

>>> validaPosTab ((4,4,'N'),(3,3))
False

-}

validaPosTab :: PosicaoTabuleiro -> Bool
validaPosTab ((x,y,o),(xx,yy)) = if x <= (xx-1) && y <= (yy-1) && x >= 0 && y >= 0 then True else False

{-| A função 'validaLetras' vai verificar se os elementos da String são letras do alfabeto, sejas estas maiúsculas ou minúsculas.
 Devolve __True__ ou __False__ respetivamente.

>>>validaLetras "aacdD"
True

>>> validaLetras "5bCaa"
False

-}                             

validaLetras :: String -> Bool
validaLetras str = and (map isLetter str)

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
                     then (x,y,(rodaDireita o))
                     else if c == 'E' 
                          then (x,y,(rodaEsquerda o))
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


----------------------------------------------------------------------------
-- x3dom
----------------------------------------------------------------------------

{-| A função 'getGrafos' lê um tabuleiro e devolve uma lista com tuplos do seguinte formato: (Int,(Int,Int)), onde o primeiro elemento corresponde ao nivel 
e o segundo á coordenada da posição. A função é executada recursivamente para todas as posições do tabuleiro.

>>> getGrafos ["aBBC","aaAD","abCC","abDE"]
[(5,(3,0)),(4,(2,0)),(2,(1,0)),(1,(0,0)),(3,(3,1)),(3,(2,1)),(2,(1,1)),(1,(0,1)),(4,(3,2)),(1,(2,2)),(1,(1,2)),(1,(0,2)),(3,(3,3)),(2,(2,3)),(2,(1,3)),(1,(0,3))]

>>> getGrafos ["aBBC"]
[(3,(3,0)),(2,(2,0)),(2,(1,0)),(1,(0,0))]

-}

getGrafos ::  Tabuleiro -> [Grafo]
getGrafos tab = aux tab (0,(length tab - 1)) []
    where
            aux []        (_,_) result =                     result
            aux ([]:t)    (x,y) result = aux t     (0,y-1)   result
            aux ((h:z):t) (x,y) result = aux (z:t) ((x+1),y) result ++ [(getLvl tab (x,y),(x,y))]

{-| A função 'getResolucao' lê um tabuleiro, uma posição, os comandos e devolve uma lista com tuplos do seguinte formato: (Int,(Int,Int),(Int)), onde o primeiro elemento corresponde ao nivel 
, o segundo á coordenada da posição e o terçeiro a situação em que a lâmpada ficou: Se acesa 1, se apagada 0, de todas as lampadas que acendeu/apagou.

>>> getResolucao  ["aBBC"] ((0,0,'E'),(4,1)) "SLALSL"
[(2,(1,0,'E')),(2,(2,0,'E')),(3,(3,0,'E'))]

-}

getResolucao ::  Tabuleiro -> PosicaoTabuleiro -> Comandos -> [(Lvl,PosicaoRobot,Int)]
getResolucao tab posI com = aux [] posI com []
    where
            aux lampsON ((x,y,o),(xx,yy)) []      result = result                     
            aux lampsON ((x,y,o),(xx,yy)) (h:t)   result | h == 'L' && elem (x,y) lampsON = aux (filter (/= (x,y)) lampsON) ((execComando tab ((x,y,o),(xx,yy)) h),(xx,yy)) t (result ++ [((getLvl tab (x,y)),(x,y,o),1)])
                                                         | h == 'L'                       = aux (lampsON ++ [(x,y)]) ((execComando tab ((x,y,o),(xx,yy)) h),(xx,yy)) t (result ++ [((getLvl tab (x,y)),(x,y,o),0)] )
                                                         | otherwise                      = aux lampsON ((execComando tab ((x,y,o),(xx,yy)) h),(xx,yy)) t result

{-| A função 'getOrient' lê um tabuleiro, uma posição e um unico comando e devolve a Orientação do robot após a execução desse comando.

>>> getOrient  ["aBBC"] ((0,0,'E'),(4,1)) 'S'
'E'

-}

getOrient :: Tabuleiro -> PosicaoTabuleiro -> Char -> Char
getOrient tab ((z,u,v),(xx,yy)) c = aux (execComando tab ((z,u,v),(xx,yy)) c)
    where
            aux (x,y,o)  = o

{-| A função 'getX' lê um tabuleiro, uma posição e um unico comando e devolve a posição em X do robot após a execução desse comando.

>>> getX  ["aBBC"] ((0,0,'E'),(4,1)) 'S'
1

-}

getX :: Tabuleiro -> PosicaoTabuleiro -> Char -> Int
getX tab ((z,u,v),(xx,yy)) c = aux (execComando tab ((z,u,v),(xx,yy)) c)
    where
            aux (x,y,o)  = x

{-| A função 'getY' lê um tabuleiro, uma posição e um unico comando e devolve a posição em X do robot após a execução desse comando.

>>> getY  ["aBBC"] ((0,0,'E'),(4,1)) 'S'
0

-}

getY :: Tabuleiro -> PosicaoTabuleiro -> Char -> Int
getY tab ((z,u,v),(xx,yy)) c = aux (execComando tab ((z,u,v),(xx,yy)) c)
    where
            aux (x,y,o)  = y

{-| A função 'getLvl' lê um tabuleiro e uma Coordenada e devolve o nivel coorespondente a essa Coordenada.

>>> getLvl  ["aBBC"] (0,0)
1

>>> getLvl  ["aBBC"] (3,0)
3

-}

getLvl :: Tabuleiro -> Coordenada -> Int --
getLvl (h:t) (x,y) = ord (toUpper posAtualX) - 64
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

{-| A função 'getLvl2' lê um tabuleiro e uma PosicaoRobot e devolve o nivel coorespondente a essa PosicaoRobot.

>>> getLvl2  ["aBBC"] (0,0,'N')
1

>>> getLvl2  ["aBBC"] (3,0,'N')
3

-}

getLvl2 :: Tabuleiro -> PosicaoRobot -> Int --
getLvl2 (h:t) (x,y,o) = ord (toUpper posAtualX) - 64
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

{-| A função 'timeAnimScene' determinda, apartir dos Comandos e de um Float que corresponde ao valor pelo qual queremos iniciar a soma, o tempo total da animação x3dom.

>>> timeAnimScene "ASSLDES" 0
7.5

-}

timeAnimScene :: Comandos -> Float -> Float
timeAnimScene [] n = n
timeAnimScene (h:t) n | h == 'E' || h == 'D' = timeAnimScene t (n+0.5)
                      | h == 'S' = timeAnimScene t (n+1.5)
                      | otherwise = timeAnimScene t (n+1)

{-| A função 'timeMoveRobot' devolve uma String com o tempo, em percentagem ( vai de 0 até 1), a que cada comando é executado.

>>>  timeMoveRobot "SAALDE"
"0 0.13636364 0.27272728 0.45454547 0.6363636 0.8181818 0.9090909 1.0 "

-}

timeMoveRobot :: Comandos -> String
timeMoveRobot com = aux com 0 "0 "
    where
            i= 1/(timeAnimScene com 0)
            aux []    n result = result
            aux (h:t) n result | h == 'E' || h == 'D' = aux t (n+(i/2)) (result ++ (show (n+(i/2))) ++ " ")
                               | h == 'S' = aux t (n+(i*1.5)) (result ++ (show (n+((i*1.5)/2))) ++ " " ++ (show (n+(i*1.5))) ++ " ")
                               | otherwise = aux t (n+i) (result ++ (show (n+i)) ++ " ")

{-| A função 'posAnimRobot' lê um Tabuleiro, a PosiçãoTabuleiro e os Comandos e devolve uma string com as coordenadas XZY de 
todas as posições por onde o robot passa com a execução dos comandos.

>>>  posAnimRobot  ["aBBC"] ((0,0,'E'),(4,1)) "SASL"
"0 0 0  0 1 0  -1 1 0  -2 1 0  -2 2 0  -3 2 0  -3 2 0  "

-}

posAnimRobot :: Tabuleiro -> PosicaoTabuleiro -> Comandos -> String
posAnimRobot tab ((z,u,v),(xx,yy)) (h:t) = aux (h:t) (z,u,v) []
    where
            aux [] (x,y,o) result = show (-z) ++ " " ++ show ((getLvl tab (z,u))-1) ++ " " ++ show u ++ "  " ++  result
            
            aux (h:t)  (x,y,o) result = if (execComando tab ((x,y,o),(xx,yy)) h) == (-1,-1,o)
                                               then if h == 'S'
                                                        then aux t (x,y,o) (result ++ show (-x)  ++ " " ++ show ((getLvl2 tab (x,y,o))-1) ++ " " ++ show (y) ++ "  " ++ show (-x)  ++ " " ++ show ((getLvl2 tab (x,y,o))-1) ++ " " ++ show (y) ++ "  ")
                                                        else aux t (x,y,o) (result ++ show (-x)  ++ " " ++ show ((getLvl2 tab (x,y,o))-1) ++ " " ++ show (y) ++ "  ")
                                                   else if h == 'S'
                                                   then if (getLvl tab (x,y)) < (getLvl2 tab (execComando tab ((x,y,o),(xx,yy)) h))
                                                            then if o == 'N'
                                                                     then aux t (execComando tab ((x,y,o),(xx,yy)) h) (result ++ (show (-x)) ++ " " ++ show ((getLvl tab (x,y+1))-1) ++ " " ++ (show y) ++ "  " ++ (show (-x)) ++ " " ++ show ((getLvl tab (x,y+1)-1)) ++ " " ++ (show (y+1))++ "  ")
                                                                     else if o == 'S'
                                                                              then aux t (execComando tab ((x,y,o),(xx,yy)) h) (result ++ (show (-x)) ++ " " ++ show ((getLvl tab (x,y-1))-1) ++ " " ++ (show y) ++ "  " ++ (show (-x)) ++ " " ++ show ((getLvl tab (x,y-1)-1)) ++ " " ++ (show (y-1))++ "  ")
                                                                              else if o == 'E'
                                                                                       then aux t (execComando tab ((x,y,o),(xx,yy)) h) (result ++ (show (-x)) ++ " " ++ show ((getLvl tab (x+1,y))-1) ++ " " ++ (show y) ++ "  " ++ (show (-(x+1))) ++ " " ++ show ((getLvl tab (x+1,y)-1)) ++ " " ++ (show y)++ "  ")
                                                                                       else if o == 'O'
                                                                                                then aux t (execComando tab ((x,y,o),(xx,yy)) h) (result ++ (show (-x)) ++ " " ++ show ((getLvl tab (x-1,y))-1) ++ " " ++ (show y) ++ "  " ++ (show (-(x-1))) ++ " " ++ show ((getLvl tab (x-1,y)-1)) ++ " " ++ (show y)++ "  ")
                                                                                                else "ERROR"
                                                            else if o == 'N'
                                                                     then aux t (execComando tab ((x,y,o),(xx,yy)) h) (result ++ (show (-x)) ++ " " ++ show ((getLvl tab (x,y))-1) ++ " " ++ (show (y+1)) ++ "  " ++ (show (-x)) ++ " " ++ show ((getLvl tab (x,y+1)-1)) ++ " " ++ (show (y+1))++ "  ")
                                                                     else if o == 'S'
                                                                              then aux t (execComando tab ((x,y,o),(xx,yy)) h) (result ++ (show (-x)) ++ " " ++ show ((getLvl tab (x,y))-1) ++ " " ++ (show (y-1)) ++ "  " ++ (show (-x)) ++ " " ++ show ((getLvl tab (x,y-1)-1)) ++ " " ++ (show (y-1))++ "  ")
                                                                              else if o == 'E'
                                                                                       then aux t (execComando tab ((x,y,o),(xx,yy)) h) (result ++ (show (-(x+1))) ++ " " ++ show ((getLvl tab (x,y))-1) ++ " " ++ (show y) ++ "  " ++ (show (-(x+1))) ++ " " ++ show ((getLvl tab (x+1,y)-1)) ++ " " ++ (show y)++ "  ")
                                                                                       else if o == 'O'
                                                                                                then aux t (execComando tab ((x,y,o),(xx,yy)) h) (result ++ (show (-(x-1))) ++ " " ++ show ((getLvl tab (x,y))-1) ++ " " ++ (show y) ++ "  " ++ (show (-(x-1))) ++ " " ++ show ((getLvl tab (x-1,y)-1)) ++ " " ++ (show y)++ "  ")
                                                                                                else "ERROR"
                                  
                                                   else aux t (execComando tab ((x,y,o),(xx,yy)) h) (result ++ (show (-(getX tab ((x,y,o),(xx,yy)) h )))  ++ " " ++ show ((getLvl2 tab (execComando tab ((x,y,o),(xx,yy)) h))-1) ++ " " ++ show (getY tab ((x,y,o),(xx,yy)) h ) ++ "  ")




{-| A função 'spinO' lê uma Orientacao e devolve uma String com a orientação coorespondente num plano do x3dom.

>>>  spinO 'O'
"0 1 0 1.57  "

-}

spinO :: Char -> String
spinO o | o == 'O' = "0 1 0 1.57  "
        | o == 'E' = "0 -1 0 1.57  "
        | o == 'N' = "0 0 0 3.14  "
        | o == 'S' = "0 1 0 3.14  "

{-| A função 'posSpinRobot' lê um Tabuleiro, uma PosicaoTabuleiro e os Comandos para develver uma String com as 
orientações coorespondentes num plano x3dom de todas as posições por onde o robot passa durante a resolução do tabuleiro.

>>>  posSpinRobot   ["aBBC"] ((0,0,'E'),(4,1)) "SADEE"
"0 -1 0 1.57  0 -1 0 1.57  0 -1 0 1.57  0 -1 0 1.57  0 1 0 3.14  0 -1 0 1.57  0 0 0 3.14  "

-}

posSpinRobot :: Tabuleiro -> PosicaoTabuleiro -> Comandos -> String
posSpinRobot tab ((z,u,v),(xx,yy)) (h:t) = aux (h:t) (z,u,v) (spinO v) []
    where
            aux [] (x,y,o) k result = result ++ spinO o
            
            aux (h:t)  (x,y,o) k result | (execComando tab ((x,y,o),(xx,yy)) h) == (-1,-1,o) && h == 'S' = aux t (x,y,o) (spinO o) (result ++ (spinO o ++ spinO o))

                                        | (execComando tab ((x,y,o),(xx,yy)) h) == (-1,-1,o)             = aux t (x,y,o) (spinO o) (result ++ spinO o)

                                        | h == 'S'  = aux t (execComando tab ((x,y,o),(xx,yy)) h) (spinO o) (result ++ spinO o ++ spinO o)

                                        | otherwise = aux t (execComando tab ((x,y,o),(xx,yy)) h) (spinO o) (result ++ spinO o)

{-| A função 'tabuleirox3dom' lê um Tabuleiro, uma PosicaoTabuleiro e os Comandos e devolve o codigo <html> coorespondente à geração do tabuleiro de jogo.

>>>  tabuleirox3dom  ["aBBC"] ((0,0,'E'),(4,1)) "SAASL"
["\n","<Transform translation='0 0 0'> <Shape USE='CUBO'/> </Transform>","<Transform translation='-1 1 0'> <Shape USE='CUBO'/> </Transform>",
"<Transform translation='-1 0 0'> <Shape USE='CUBO'/> </Transform>","<Transform translation='-2 1 0'> <Shape USE='CUBO'/> </Transform>",
"<Transform translation='-2 0 0'> <Shape USE='CUBO'/> </Transform>","<Transform translation='-3 2 0'> <Shape USE='CUBO'/> </Transform>",
"<Transform translation='-3 1 0'> <Shape USE='CUBO'/> </Transform>","<Transform translation='-3 0 0'> <Shape USE='CUBO'/> </Transform>"]

-}


tabuleirox3dom :: Tabuleiro -> PosicaoTabuleiro -> Comandos -> PosicaoLampadas -> [String]
tabuleirox3dom tab ((x,y,o),(xx,yy)) com posL = (aux (reverse (getGrafos tab)) 1 [] 1) ++ (lampadas posL (getResolucao tab ((x,y,o),(xx,yy)) com) 1 [])
    where
            aux []            i result p = ["\n"] ++ result
            aux ((n,(x,y)):t) i result p = if n > 0
                                          then if isUpper (verificaNivel tab (x,y)) == True && p == 1
                                               then aux ((n-1,(x,y)):t) (i+1) (result ++ ["<Transform translation='"++(show (-x))++" "++(show (n-1))++" "++(show y)++"'> <Shape USE='LAMPADA'/> </Transform>"]) 0
                                               else aux ((n-1,(x,y)):t) i     (result ++ ["<Transform translation='"++(show (-x))++" "++(show (n-1))++" "++(show y)++"'> <Shape USE='CUBO'/> </Transform>"]) 0
                                          else aux t i result 1

            lampadas posL []                 i result = result
            lampadas posL ((n,(z,u,v),k):xs) i result = if elem (z,u) posL
                                                     then lampadas (filter (/= (z,u)) posL) xs (i+1) (result ++ ["<Transform translation='"++(show (-z))++" "++(show (n-1))++" "++(show u)++"'> <Shape USE='LAMP"++ show i++"'/> </Transform>"])
                                                     else lampadas posL xs (i+1) result


{-| A função 'posIniX3dom' lê um Tabuleiro, uma PosicaoTabuleiro e devolve como String a posição inicial do robot em formato de coordenada num plano x3dom (XZY).

>>>  posIniX3dom   ["aBBC"] ((2,0,'E'),(4,1))
"2 1 0"

-}

posIniX3dom :: Tabuleiro -> PosicaoTabuleiro -> String
posIniX3dom  tab ((x,y,o),(xx,yy)) = show x ++ " " ++ show (nivel-1) ++ " " ++ show y 
    where 
            nivel = (getLvl tab (x,y))

{-| A função 'lampsX3dom' lê um Tabuleiro, uma PosicaoTabuleiro e os Comandos e devolve o codigo <html> correspondente ás criação das lampadas no tabuleiro.

-}

lampsX3dom :: Tabuleiro -> PosicaoTabuleiro -> Comandos -> IO String
lampsX3dom tab ((x,y,o),(xx,yy)) com  = aux (posicaoLampadas tab (xx,yy)) (getResolucao tab ((x,y,o),(xx,yy)) com) 1 [] 
    where 
                        i= 1/(timeAnimScene com 0)

                        keylamp (j,g,q) posL (z,u,v) n numL []     tempoT result re2 = if q == 1
                                                                                           then "0 " ++ result ++ "1'" ++ " \nkeyValue='230 0 0  " ++ re2 ++ "1 1 0'"
                                                                                           else "0 " ++ result ++ "1'" ++ " \nkeyValue='230 0 0  " ++ re2 ++ "230 0 0'"
                        keylamp (j,g,q) posL (z,u,v) n numL (h:t)  tempoT result re2     
                                                                         | h == 'L' =  if (j,g) == (z,u) && n == numL
                                                                                           then if q == 0
                                                                                                    then keylamp (j,g,1) posL (execComando tab ((z,u,v),(xx,yy)) h) n numL t (tempoT+i) (result ++ show tempoT ++ " " ++ show (tempoT + (i/4))++ " ") (re2 ++ "230 0 0  1 1 0  ")
                                                                                                    else keylamp (j,g,0) posL (execComando tab ((z,u,v),(xx,yy)) h) n numL t (tempoT+i) (result ++ show tempoT ++ " " ++ show (tempoT + (i/4))++ " ") (re2 ++ "1 1 0  230 0 0  ")
                                                                                           else keylamp (j,g,q) posL (execComando tab ((z,u,v),(xx,yy)) h) n (numL+1) t (tempoT+i) result re2
                                                                         
                                                                         | (h == 'E' || h == 'D') = keylamp (j,g,q) posL (execComando tab ((z,u,v),(xx,yy)) h) n numL t (tempoT+(i/2)) result re2
                                                                         | h == 'S'               = keylamp (j,g,q) posL (execComando tab ((z,u,v),(xx,yy)) h) n numL t (tempoT+(i*1.5)) result re2
                                                                         | otherwise              = keylamp (j,g,q) posL (execComando tab ((z,u,v),(xx,yy)) h) n numL t (tempoT+i) result re2
                                                            
                        

                        aux posL []            i result = return result
                        aux posL ((n,(k,p,f),q):t) i result = do
                                                                lamp_shape <- readFile "X3dom/shapes/cubo"
                                                                if elem (k,p) posL
                                                                then aux (filter (/= (k,p)) posL) t (i+1) (result ++ "<Group DEF='LAMP"++ show i ++"'>\n" ++ lamp_shape ++ "\n    <Shape>\n        <Appearance>\n            <Material DEF='lampON"++ show i ++"' diffuseColor='#E60000'/>\n        </Appearance>\n        <Box size='.98 .98 .98'></Box>\n    </Shape>\n\n" ++ "<PositionInterpolator DEF='lampColor"++ show i ++"' \nkey='"++ (keylamp (k,p,q) posL (x,y,o) i 1 com  0  [] []) ++">\n</PositionInterpolator>\n" ++ "\n</Group>\n\n\n")
                                                                else aux posL t (i+1) result

{-| A função 'chapeuX3dom' lê um Tabuleiro, uma PosicaoTabuleiro e os Comandos para devolver como String o codigo <html> coorespondente à animação da bola do chapeu do robot.

>>>  chapeuX3dom   ["aBbC"] ((0,0,'E'),(4,1)) "SLAASL"
"\n\n<PositionInterpolator DEF='colorChangeChapeu' \nkey='0 0.14285716 0.17857145 0.25000003 0.2857143 0.78571427 0.8214286 0.89285713 1' 
\nkeyValue='1 1 1  1 1 1  1 1 0  1 1 0  1 1 1  1 1 1  1 1 0  1 1 0  1 1 0'>\n</PositionInterpolator>"

-}

chapeuX3dom :: Tabuleiro -> PosicaoTabuleiro -> Comandos -> IO String
chapeuX3dom tab ((x,y,o),(xx,yy)) com  = aux com []
    where 
                        i= 1/(timeAnimScene com 0)

                        keychapeu []     tempoT result = "0 " ++ result ++ "1"
                        keychapeu (h:t)  tempoT result  | h == 'L' = if t == []
                                                                         then keychapeu t (tempoT+i) (result ++ show (tempoT - (i/4)) ++ " " ++ show (tempoT - (i/1)) ++ " " ++ show (tempoT + (i/2)) ++ " ")
                                                                         else keychapeu t (tempoT+i) (result ++ show (tempoT - (i/4)) ++ " " ++ show (tempoT - (i/1)) ++ " " ++ show (tempoT + (i/2)) ++ " " ++ show (tempoT + (i/1)) ++ " ")
                                                        | (h == 'E' || h == 'D') = keychapeu t (tempoT+(i/2)) result
                                                        | h == 'S' = keychapeu t (tempoT+(i*1.5)) result
                                                        | otherwise = keychapeu t (tempoT+i) result
                                                            
                        keyvaluechapeu []     tempoT result = "1 1 1  " ++ result ++ "1 1 0"
                        keyvaluechapeu (h:t)  tempoT result | h == 'L' = if t == []
                                                                             then keyvaluechapeu t (tempoT+i) (result ++ "1 1 1  " ++ "1 1 0  " ++ "1 1 0  ")
                                                                             else keyvaluechapeu t (tempoT+i) (result ++ "1 1 1  " ++ "1 1 0  " ++ "1 1 0  " ++ "1 1 1  ")
                                                            | (h == 'E' || h == 'D') = keyvaluechapeu t (tempoT+(i/2)) result
                                                            | h == 'S' = keyvaluechapeu t (tempoT+(i*1.5)) result
                                                            | otherwise = keyvaluechapeu t (tempoT+i) result
                        

                        aux (h:t) result = return (result ++ "\n\n<PositionInterpolator DEF='colorChangeChapeu' \nkey='"++ (keychapeu com  0  []) ++"' \nkeyValue='"++ (keyvaluechapeu com  0  []) ++"'>\n</PositionInterpolator>")

{-| A função 'routeLamps' recebe os Comandos e devolve uma String com o codigo <html> coorespondente à routa de animação das lâmpadas do tabuleiro.

-}

routeLamps :: PosicaoLampadas -> [(Lvl,PosicaoRobot,Int)] -> String
routeLamps posL res  = aux posL res 1 []
    where
            aux posL [] n result = result
            aux posL  ((z,(k,p,f),q):t) n result = if elem (k,p) posL
                                                   then aux (filter (/= (k,p)) posL) t (n+1) (result ++ "\n\n<Route fromNode='time' fromField ='fraction_changed' toNode='lampColor"++ show n ++"' toField='set_fraction'></Route>\n<Route fromNode='lampColor"++ show n ++"' fromField ='value_changed' toNode='lampON"++ show n ++"' toField='diffuseColor'></Route>\n")
                                                   else aux posL t (n+1) result

{-| A função 'timeSensorX3dom' recebe os Comandos e devolve uma String com o codigo <html> coorespondente ao tempo de animação.

-}

timeSensorX3dom :: Comandos -> String
timeSensorX3dom com = "\n<timeSensor DEF='time' cycleInterval='"++show (timeAnimScene com 0)++"' loop='true'></timeSensor>\n"

{-| A função 'starText' devolve uma string mediante o resultado final da resolução dos comandos fornecidos.

-}

starText :: Tabuleiro -> PosicaoTabuleiro -> Comandos -> String
starText tab ((x,y,o),(xx,yy)) com  = aux (getResolucao tab ((x,y,o),(xx,yy)) com) [] []
   where
          aux []                lampsON result = if lampsON ==  []
                                                     then result ++ "PRÓXIMO NÍVEL   "
                                                     else result ++ "TENTE DE NOVO   "
          aux ((n,(x,y,o),q):t) lampsON result = if q == 0
                                                     then aux t (filter (/= (x,y)) lampsON) result
                                                     else aux t (lampsON ++ [(x,y)]) result

{-| A função 'comandGerLid' devolve uma string dependendo se estamos a ler comandos do input ou se estamos a gerar comandos automáticamente.

-}

comandGerLid :: [String] -> String
comandGerLid com  | com == [] = "Comandos Gerados: "
				  | otherwise = "Comandos Lidos: "
		                                                    

{-| A função 'criaX3domFile' lê um Tabuleiro, uma PosicaoTabuleiro e os Comandos e vai devolver todo o codigo <html> correspondente à apresentação gráfica daquele jogo.

-}

criaX3domFile :: Tabuleiro -> PosicaoTabuleiro -> Comandos -> String -> IO ()
criaX3domFile tab ((x,y,o),(xx,yy)) com a = do
                                                star_shape <- readFile "X3dom/shapes/star_shape"
                                                robot_shape <- readFile "X3dom/shapes/robot_shape"
                                                cubo_shape <- readFile "X3dom/shapes/cubo"
                                                lampada_shape <- readFile "X3dom/shapes/lampada"
                                                lamp <- lampsX3dom tab ((x,y,o),(xx,yy)) com
                                                chapeu_lamp <- chapeuX3dom tab ((x,y,o),(xx,yy)) com
                                                let robot_chapeu_route = "\n<Route fromNode='time' fromField ='fraction_changed' toNode='colorChangeChapeu' toField='set_fraction'></Route>\n<Route fromNode='colorChangeChapeu' fromField ='value_changed' toNode='yellow' toField='diffuseColor'></Route>\n"
                                                let robot_move = "\n\n<PositionInterpolator DEF='move' \nkey='" ++ timeMoveRobot com ++ "' \nkeyValue='"++ posAnimRobot tab ((x,y,o),(xx,yy)) com ++"'>\n</PositionInterpolator>\n"
                                                let robot_move_route = "\n<Route fromNode='time' fromField ='fraction_changed' toNode='move' toField='set_fraction'></Route>\n<Route fromNode='move' fromField ='value_changed' toNode='paiNatal' toField='set_translation'></Route>\n"
                                                let robot_spin = "\n<OrientationInterpolator DEF='spin' \nkey='" ++ timeMoveRobot com ++ "' \nkeyValue='" ++ posSpinRobot tab ((x,y,o),(xx,yy)) com  ++   "'>\n</OrientationInterpolator>\n"
                                                let robot_spin_route = "\n<Route fromNode='time' fromField ='fraction_changed' toNode='spin' toField='set_fraction'></Route>\n<Route fromNode='spin' fromField ='value_changed' toNode='paiNatal' toField='set_rotation'></Route>\n"
                                                let star_route = "\n<Route fromNode='time' fromField ='fraction_changed' toNode='starChange' toField='set_fraction'></Route>\n<Route fromNode='starChange' fromField ='value_changed' toNode='star' toField='set_translation'></Route>\n"
                                                putStr

                                                      ("<!DOCTYPE html>\n<html>\n\n"
                                                      ++ "<head>\n"    
                                                      ++ "<meta http-equiv='X-UA-Compatible' content='IE=edge'/>\n"    
                                                      ++ "<meta charset='UTF-8'>\n"
                                                      ++ "<title>Lightbot</title>\n"
                                                      ++ "<script type='text/javascript' src='http://www.x3dom.org/download/x3dom.js'> </script>\n"
                                                      ++ "<link rel='stylesheet' type='text/css' href='http://www.x3dom.org/download/x3dom.css'/>\n"
                                                      ++ "<link rel='stylesheet' type='text/css' href='http://www.rpagb.ddns@rpagb.ddns.net/web/UM/style.css'/>\n"
                                                      ++ "<script src='http://www.rpagb.ddns@rpagb.ddns.net/web/UM/snowstorm.js'></script>\n"
                                                      ++ "</head>\n\n"
                                                      ++ "<body>\n\n"
                                                      
                                                      ++ "<div class='x3dom'>\n<x3d width='800px' height='600px'>\n<scene>\n\n"
                                                      ++ "<Viewpoint id='view' centerOfRotation='0,0,0' position='-12 15 15' orientation='3 4 2 -1' description='camera'></Viewpoint>\n"
                                                      
                                                      ++ "\n\n<Group>\n    <transform DEF='star' translation='7 7 4' scale='0.9 0.9 0.9'>\n        <Transform translation='-1 0 -3.5' rotation='0 -1 0 1.14'>\n            <Shape>\n                <Text string='" ++  starText tab ((x,y,o),(xx,yy)) com ++ "'>\n"
                                                      ++ star_shape
                                                      ++ "\n\n<Group>\n\n<transform DEF='paiNatal' scale='0.55 0.7 0.7' translation='"++posIniX3dom tab ((x,y,o),(xx,yy))++"'>\n" 
                                                      ++ robot_shape
                                                      ++ chapeu_lamp
                                                      ++ robot_move
                                                      ++ robot_spin
                                                      ++ "\n</Group>\n\n\n"

                                                      ++ "<Group DEF='CUBO'>\n" 
                                                      ++ cubo_shape
                                                      ++ "</Transform>\n        <Shape>\n        <Appearance>\n            <Material diffuseColor='#1F7A1F'/>\n        </Appearance>\n        <Box size='.98 .98 .98'/>\n    </Shape>\n</Group>\n\n\n"

                                                      ++ "<Group DEF='LAMPADA'>\n" 
                                                      ++ lampada_shape
                                                      ++ "</Transform>\n        <Shape>\n        <Appearance>\n            <Material diffuseColor='#E60000'/>\n        </Appearance>\n        <Box size='.98 .98 .98'/>\n    </Shape>\n</Group>\n\n\n"

                                                      ++ lamp                                         
                                                      
                                                      ++ outStr (tabuleirox3dom tab ((x,y,o),(xx,yy)) com (posicaoLampadas tab (xx,yy)))
                                                      ++ timeSensorX3dom com

                                                      ++ robot_chapeu_route
                                                      ++ robot_move_route
                                                      ++ robot_spin_route
                                                      ++ star_route
                                                      ++ routeLamps (posicaoLampadas tab (xx,yy)) (getResolucao tab ((x,y,o),(xx,yy)) com)
                                                      
                                                      ++ "\n\n<Background skyColor='#000000'/>\n\n</scene>\n</x3d>\n"
                                                      ++ "</div>\n\n\n"

                                                      ++ "<div class='box-right'>\n"
                                                      ++ "    <h1>LightBot</h1>\n"
                                                      ++ "    <h1>Edição de Natal</h1>\n"
                                                      ++ "    <h2>Tabuleiro de Jogo: <b>[" ++ outStr tab ++ "]</b></h2>\n"
                                                      ++ "    <h2>Posição Inicial: <b>" ++ show (x,y,o) ++ "</b></h2>\n"
                                                      ++ "    <h2>" ++ a ++ "<b>[" ++ com ++ "]</b></h2>\n"
                                                      ++ "    <br><br>\n"
                                                      ++ "    <h2>Realizado por: <br><b>Nuno Armada A75877 | Paulo Franco  A75871</b></h2>\n"
                                                      ++ "    <h2>No ambito do projeto: LightBot em Haskell</h2>\n"
                                                      ++ "    <h2>Licenciatura Engenharia Informática</h2>\n"
                                                      ++ "    <h2>Universidade do Minho | 2014</h2>\n"
                                                      ++ "</div>\n\n\n"


                                                      ++ "</body>\n</html>")