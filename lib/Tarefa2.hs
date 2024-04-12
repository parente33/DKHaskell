{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Daniel Gonçalves Parente <a107363@alunos.uminho.pt>
              Tiago Silva Martins <a106938@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Tarefa1

-- verifica se um dado jogo não viola nenhuma das restrições impostas abaixo
valida :: Jogo -> Bool
valida jogo@(Jogo mapa inimigos colecionaveis jogador) = chaoDePlataforma mapa && jogadorMenosLargo jogador && apenasinimigosRessaltam inimigos jogador
    && fantasmasTemUmaVida inimigos && numeroMinInimigos inimigos && numeroMinColecionaveis colecionaveis && semColisoesIniciais jogador inimigos
    && validaEscadas mapa && inimigosSobBlocos mapa inimigos && jogadorSobBloco mapa jogador && colecionaveisNosVazios mapa colecionaveis


-- verifica se um nível do mapa é todo constituído por blocos de Plataforma
tudoPlataforma :: [Bloco] -> Bool
tudoPlataforma [] = True
tudoPlataforma (h:t)
    | h==Plataforma = tudoPlataforma t
    | otherwise = False

-- verifica se o chão do mapa é todo constituído por blocos de Plataforma
chaoDePlataforma :: Mapa -> Bool
chaoDePlataforma (Mapa _ _ lblocos) = tudoPlataforma (last lblocos)

-- o jogador não pode ser mais largo que um bloco do tipo Alcapao (todos os blocos têm tamanho (1,1))
jogadorMenosLargo :: Personagem -> Bool
jogadorMenosLargo jogador@Personagem {tipo = Jogador, tamanho = (x,y)} = x>0 && x<=1 && y>0 && y<=1

-- todos os inimigos têm a propriedade ressaltam a True, o jogador têm-na a False (Indica se o personagem deve trocar de direcção ao colidir com uma parede 
-- ou ao chegar ao fim de uma plataforma)
apenasinimigosRessaltam :: [Personagem] -> Personagem -> Bool
apenasinimigosRessaltam linimigos jogador = all inimigosRessaltam linimigos && jogadorRessalta jogador
    where 
        inimigosRessaltam i = ressalta i
        jogadorRessalta j = not (ressalta j)

-- todos os fantasmas têm uma vida só
fantasmasTemUmaVida :: [Personagem] -> Bool
fantasmasTemUmaVida = all vidaUnicaFantasmas
    where
        vidaUnicaFantasmas p = eFantasma p && vidaUnica p
        eFantasma p = tipo p == Fantasma
        vidaUnica p = vida p == 1

-- há, no início do jogo, 4 inimigos, no mínimo
numeroMinInimigos :: [Personagem] -> Bool
numeroMinInimigos inimigos = length inimigos >=4

-- há, no início do jogo, 5 colecionáveis (moedas + martelos), no mínimo
numeroMinColecionaveis :: [(Colecionavel,Posicao)] -> Bool
numeroMinColecionaveis colecionaveis = length colecionaveis >=5

-- não existem colisões entre o jogador e qualquer inimigo no início do jogo
semColisoesIniciais :: Personagem -> [Personagem] -> Bool
semColisoesIniciais _ [] = True
semColisoesIniciais j (h:t) = not (colisoesPersonagens j h) && semColisoesIniciais j t

-- as escadas do jogo têm que ter um bloco do tipo Plataforma no seu começo e/ou no seu fim, e não podem começar nem acabar noutros tipos de blocos
-- (a não ser os do tipo Vazio)
validaEscadas :: Mapa -> Bool
validaEscadas (Mapa _ _ []) = True
validaEscadas mapa@(Mapa _ _ matriz) = escadasEalcapoesValidos (indicesBlocosMapa mapa)
    where
        -- verifica se as escadas não terminam ou começam em alçapões, e numa das extremidades há um bloco do tipo Plataforma
        escadasEalcapoesValidos :: [((Int,Int),Bloco)] -> Bool
        escadasEalcapoesValidos [] = True
        escadasEalcapoesValidos (((x,y),bloco):t)
            | bloco == Escada = verificaAlcapaoEmCima t (x-1,y) && verificaAlcapaoEmBaixo t (x+1,y) && escadasEalcapoesValidos t
            | otherwise = escadasEalcapoesValidos t

        -- ao andar para cima nas escadas não encontramos um alçapão
        verificaAlcapaoEmCima :: [((Int,Int),Bloco)] -> (Int,Int) -> Bool
        verificaAlcapaoEmCima (((x,y),bloco):t) (x1,y1)
            | bloco == Alcapao && x==x1 && y==y1 = False
            | bloco == Vazio && naoPlataformaAbaixo ((x+1,y),bloco) = False
            | otherwise = True

        -- ao andar para baixo nas escadas não encontramos um alçapão
        verificaAlcapaoEmBaixo :: [((Int,Int),Bloco)] -> (Int,Int) -> Bool
        verificaAlcapaoEmBaixo (((x,y),bloco):t) (x1,y1)
            | bloco == Alcapao && x==x1 && y==y1 = False
            | bloco == Escada = verificaAlcapaoEmCima (((x+1,y),bloco):t) (x1,y1)
            | bloco == Vazio && naoPlataformaAcima ((x-1,y),bloco) = False
            | otherwise = True

        -- verifica se não existe um bloco do tipo Plataforma acima das escadas
        naoPlataformaAcima :: ((Int,Int),Bloco) -> Bool
        naoPlataformaAcima ((x,y),bloco)
            | bloco == Escada = naoPlataformaAcima ((x-1,y),bloco)
            | bloco == Vazio = True
            | bloco == Plataforma = False
            | otherwise = True

        -- verifica se não existe um bloco do tipo Plataforma por baixo das escadas
        naoPlataformaAbaixo :: ((Int,Int),Bloco) -> Bool
        naoPlataformaAbaixo ((x,y),bloco)
            | bloco == Escada = naoPlataformaAbaixo ((x+1,y),bloco)
            | bloco == Vazio = True
            | bloco == Plataforma = False
            | otherwise = True

-- verifica se todos os inimigos não estão dentro de blocos do tipo Plataforma ou Alcapao
inimigosSobBlocos :: Mapa -> [Personagem] -> Bool
inimigosSobBlocos (Mapa _ _ []) _ = True
inimigosSobBlocos _ [] = True
inimigosSobBlocos mapa@(Mapa _ _ matriz) (h:t) = colisaoComBlocos mapa h && inimigosSobBlocos mapa t

-- verifica se o jogador não está dentro de um bloco do tipo Plataforma ou Alcapao
jogadorSobBloco :: Mapa -> Personagem -> Bool
jogadorSobBloco (Mapa _ _ []) _ = True
jogadorSobBloco mapa@(Mapa _ _ matriz) jogador = colisaoComBlocos mapa jogador

-- verifica se todos os colecionáveis do mapa se encontram em blocos do tipo Vazio
colecionaveisNosVazios :: Mapa -> [(Colecionavel,Posicao)] -> Bool
colecionaveisNosVazios _ [] = True
colecionaveisNosVazios mapa ((c,(x,y)):t) = colecionaveis' (indicesBlocosMapa mapa) (c,(x,y)) && colecionaveisNosVazios mapa t
    where
        colecionaveis' :: [((Int,Int),Bloco)] -> (Colecionavel,Posicao) -> Bool
        colecionaveis' [] _ = True
        colecionaveis' (((x,y),bloco):t) (c,(a,b))
            | a == fromIntegral x && b == fromIntegral y && bloco == Vazio = colecionaveis' t (c,(a,b))
            | otherwise = False



