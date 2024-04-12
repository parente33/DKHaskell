{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Daniel Gonçalves Parente <a107363@alunos.uminho.pt>
              Tiago Silva Martins <a106938@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe

import LI12324
import Tarefa1

-- função auxiliar da autalizaestado
atualizajogo :: Maybe Acao -> Jogo -> Jogo
atualizajogo a j@Jogo { jogador = mario, mapa= mapa } =
  let jogadorAtualizado = atualizaPersonagem a mario mapa
  in j {jogador = jogadorAtualizado }

-- atualiza o estado do jogo tendo em conta as ações das personagens
atualizaestado :: Maybe Acao -> Estado -> Estado
atualizaestado a e =
  let jogoAtualizado = atualizajogo a (jogo e)
  in e { jogo = jogoAtualizado }

-- função geral da tarefa
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoesInimigos acaoJogador jogo@(Jogo mapa inimigos colecionaveis jogador) =
      let novoJogador = atualizaPersonagem acaoJogador jogador mapa
          novosInimigos = zipWith atualizaInimigos acoesInimigos inimigos
      in jogo {jogador = novoJogador,inimigos = novosInimigos}

-- atualiza os vetores velocidade do jogador consoante a ação que efetua
atualizaPersonagem :: Maybe Acao -> Personagem -> Mapa -> Personagem
atualizaPersonagem acao jogador map = case acao of
                                      Just Subir -> if emEscada jogador
                                                      then jogador {posicao = (x,y-0.5), velocidade = (0,-7) ,direcao = Norte}
                                                      else jogador
                                      Just Descer -> if emEscada jogador
                                                      then jogador {posicao = (x,y+0.5), velocidade = (0,7) ,direcao = Sul}
                                                      else jogador
                                      Just AndarDireita -> if 1==1  -- condição precisa ser sempre verdadeira
                                                            then jogador {velocidade = (5,vy),direcao = Este}
                                                            else jogador
                                      Just AndarEsquerda -> if 1==1 -- condição precisa ser sempre verdadeira
                                                            then jogador {velocidade = (-5,vy),direcao = Oeste}
                                                            else jogador
                                      Just Saltar -> if vy == 0 && not (colisaoComEscada map jogador)
                                                      then jogador {velocidade = (vx,-13)}
                                                      else jogador
                                      Just Parar -> if vx /= 0
                                                      then jogador {velocidade = (0,vy)}
                                                      else jogador
                                      Nothing -> jogador
                      where (vx,vy) = velocidade jogador
                            (x,y) = posicao jogador

-- atualiza os vetores velocide do inimigo consoante a ação que efetua
atualizaInimigos :: Maybe Acao -> Personagem -> Personagem
atualizaInimigos acao inimigo = case acao of
                                  Just Subir -> if emEscada inimigo
                                                      then inimigo {posicao = (x,y-0.5), velocidade = (0,-1),direcao = Norte}
                                                      else inimigo
                                  Just Descer -> if emEscada inimigo
                                                      then inimigo {posicao = (x,y-0.5), velocidade = (0,1),direcao = Sul}
                                                      else inimigo
                                  Just AndarDireita -> if not (emEscada inimigo)
                                                            then inimigo {velocidade = (1,0),direcao = Este}
                                                            else inimigo
                                  Just AndarEsquerda -> if not (emEscada inimigo)
                                                            then inimigo {velocidade = (-1,0),direcao = Oeste}
                                                            else inimigo
                                  Just Parar -> if vy < 0
                                                      then inimigo {velocidade = (vx,0)}
                                                      else inimigo
                                  Nothing -> inimigo
  where
      (x,y) = posicao inimigo
      (vx,vy) = velocidade inimigo