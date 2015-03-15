----------------------------------------------------------------------------
--                               L I G H T B O T
----------------------------------------------------------------------------

{-| __TAREFA D__

Deve realizar um programa que sintetize uma sequência de comandos para o robot LightBot de forma a que este, partindo da posição inicial, 
acenda todas as lâmpadas disponíveis no tabuleiro. 

O programa deve ler do stdin um ficheiro de texto com a descrição do tabuleiro e o estado 
inicial do robot (conforme formato especificado no enunciado), assumindo que o formato dos dados de entrada é válido. 

Como resultado, 
o programa deve imprimir uma única linha no stdout contendo o programa gerado.

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
    putStr (outStr (tarefaD (lines inp)))

----------------------------------------------------------------------------
-- TAREFA D
----------------------------------------------------------------------------

{-|  A função 'tarefaD' serve para gerar todos os comandos necessários para a resolução do tabuleiro. 
Esta recebe um input com o tabuleiro e a posição inicial e devolve a linha de comandos correspondente.

>>>  tarefaD ["AbB","AAa","0 0 E"]
["LEALDSALDSDAL"]

>>> tarefaD ["zzz","Dcb","aaa","0 0 E"]
["AAESESSL"]

-}

tarefaD :: [String] -> [String]
tarefaD l = [result]
 where 
       tabuleiro   = takeWhile validaLetras l 
       coordenadas = take 1 (dropWhile validaLetras l) 
       pInicial    = posInicial coordenadas
       
       tamTab      = tamTabuleiro tabuleiro
       
       posiLamp    = posicaoLampadas tabuleiro tamTab

       result      = resolveTabul tabuleiro (pInicial,tamTab) posiLamp

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
			r4 = 	if execComando tab ((x,y,'S'),(xx,yy)) 'S' /= (-1,-1,'S') then [(x,y-1)] else []
			r5 = 	if execComando tab ((x,y,'E'),(xx,yy)) 'A' /= (-1,-1,'E') then [(x+1,y)] else []
			r6 = 	if execComando tab ((x,y,'E'),(xx,yy)) 'S' /= (-1,-1,'E') then [(x+1,y)] else []
			r7 = 	if execComando tab ((x,y,'O'),(xx,yy)) 'A' /= (-1,-1,'O') then [(x-1,y)] else []
			r8 = 	if execComando tab ((x,y,'O'),(xx,yy)) 'S' /= (-1,-1,'O') then [(x-1,y)] else []
								     
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