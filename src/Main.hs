module Main where

import LI12324
import Tarefa1 ()
import Tarefa2 ()
import Tarefa3 ()
import Tarefa4 ()
import Tarefa5
import Graphics.Gloss.Interface.IO.Game ( playIO )
import Graphics.Gloss
import SDL.Mixer
import System.Environment (getEnv)
import System.FilePath ((</>))
import Control.Exception (bracket)
import Control.Concurrent (threadDelay,forkIO)
import Control.Monad
import Control.Concurrent.STM

playIfNotPlaying :: Maybe Music -> IO ()
playIfNotPlaying (Just music) = do
    closeAudio
    playMusic Forever music
playIfNotPlaying Nothing = return ()

main :: IO ()
main = withAudio defaultAudio 256 $ do
    imgs <- carregarImagens

    curseOfArosMusic <- load "ost/Curse Of Aros - (COA) -Theme Song!.mp3" :: IO Music
    pouMusic <- load "ost/Pou music ost - ConnectCliff JumpCliff DashJet Pou.mp3" :: IO Music

    let initialMode = MenuInicial Jogar
        estadoInicial = Estado 
          { imagens = imgs,
            modo = initialMode,
            acao = Parar,
            jogo = jogo1,
            musicaMenu = Just curseOfArosMusic,
            musicaJogo = Nothing
          }

    playMusic Forever curseOfArosMusic

    playIO
        janela
        corFundo
        frameRate
        estadoInicial
        (\estado -> desenha estado (jogador (jogo estado)) 0)
        reage
        tempo