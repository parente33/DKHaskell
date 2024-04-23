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
import Control.Concurrent (threadDelay)
import Control.Monad

main :: IO ()
main = do
    imgs <- carregarImagens

    initialize [InitMP3]

    openAudio defaultAudio 256

    musicFilePath <- load "Pou music ost - ConnectCliff JumpCliff DashJet Pou.mp3"

    let musicPlaying = True

    SDL.Mixer.play musicFilePath

    let loop = do
          when musicPlaying $ do
              threadDelay 1000000
              loop

    loop

    closeAudio

    playIO
        janela
        corFundo
        frameRate
        (Estado {imagens = imgs,modo = MenuInicial Jogar,acao = Parar,jogo = jogo1})
        (\estado -> desenha estado (jogador (jogo estado)) 0)
        reage
        tempo