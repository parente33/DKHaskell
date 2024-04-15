module Main where

import LI12324
import Tarefa1 () 
import Tarefa2 ()
import Tarefa3 ()
import Tarefa4 ()
import Tarefa5 
import Graphics.Gloss.Interface.IO.Game ( playIO )
import Graphics.Gloss

main :: IO ()
main = do
    imgs <- carregarImagens
    
    playIO
        janela
        corFundo
        frameRate
        (Estado {imagens = imgs,modo = MenuInicial Jogar,acao = Parar,jogo = jogo1})
        (\estado -> desenha estado (jogador (jogo estado)) 0)
        reage
        tempo
