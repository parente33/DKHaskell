{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Daniel Gonçalves Parente <a107363@alunos.uminho.pt>
              Tiago Silva Martins <a106938@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Tarefa1


-- função geral da tarefa
movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta s t jogo@(Jogo mapa inimigos colecionaveis jogador) =
    let novoMapa = colisaoComAlcapao jogo
        novoJogo = jogo {mapa = novoMapa}
        jogoFinal = verificaColisaoMoeda $ verificaColisaoMartelo (t/10) novoJogo
    in jogoFinal


atualizaPosicaoMario :: Float -> Jogo -> Jogo
atualizaPosicaoMario dt e@Jogo {mapa = mapaAtual, jogador = mario} =
  let marioCaido = mario
      (vx,vy) = velocidade marioCaido
      (x,y) = posicao mario
      dir = direcao mario
      novaPosicao = (x+vx*realToFrac dt,y+vy*realToFrac dt)
      novaPosicao1 = (x+vx*realToFrac dt,0.5+realToFrac (truncate(y+vy*realToFrac dt)))
      novoMario = personagemCai dir mapaAtual (mario {posicao = novaPosicao, emEscada = False})
      novoMario1 = personagemCai dir mapaAtual (mario {posicao = novaPosicao1, emEscada = False})
      novoMarioEscada = personagemCai dir mapaAtual (mario {posicao = novaPosicao, emEscada = True})
      novoMarioEscada1 = personagemCai dir mapaAtual (mario {posicao = novaPosicao, emEscada = True})
      indices = devolveIndices $ filtraBlocosEscada $ indicesBlocosMapa mapaAtual
  in
         if colisaoComBlocos mapaAtual mario && vy == 0 
            then
              if colisaoComBlocos' (blocosSobreEscada mapaAtual ++ indices) mario 
                then
                    e {jogador = novoMarioEscada1}
                else
                    e {jogador = novoMario1}
            else
              if colisaoComBlocos' (blocosSobreEscada mapaAtual ++ indices) mario 
                then
                   e {jogador = novoMarioEscada}
                else
                   e {jogador = novoMario}

gravidadeAtua :: Personagem -> Mapa -> Bool
gravidadeAtua jogador mapa = not (colisaoComBlocos mapa jogador)

gravidadeAtuaE :: Personagem -> Mapa -> Bool
gravidadeAtuaE jogador mapa = not (colisaoComBlocos' (blocosSobreEscada mapa++indices) jogador)
       where indices = devolveIndices $ filtraBlocosEscada $ indicesBlocosMapa mapa

personagemCai:: Direcao -> Mapa -> Personagem -> Personagem
personagemCai dir mapa jogador
       | gravidadeAtua jogador mapa && gravidadeAtuaE jogador mapa = jogador {velocidade = novaVelocidade}
       | otherwise = jogador {velocidade =(vx,0)}
      where
    (vx,vy) = velocidade jogador
    (x,y) = posicao jogador
    novaVelocidade = (vx,vy+0.1)
    novajogador1 = jogador {velocidade =(0,vy)}
    novojogador = jogador {velocidade =(vx,0)}
