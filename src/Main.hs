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

modoToMusic :: Modo -> FilePath
modoToMusic (MenuInicial _) = "ost/Curse Of Aros - (COA) -Theme Song!.mp3"
modoToMusic EmJogo = "ost/Pou music ost - ConnectCliff JumpCliff DashJet Pou.mp3"

startMusicForMode :: Modo -> IO ()
startMusicForMode mode = do
    pararMusicaAtual
    let musicPath = modoToMusic mode
    musicFilePath <- load musicPath
    _ <- forkIO $ SDL.Mixer.play musicFilePath
    threadDelay 1

main :: IO ()
main = do
    imgs <- carregarImagens

    initialize [InitMP3]
    openAudio defaultAudio 256

    let initialMode = MenuInicial Jogar
    startMusicForMode initialMode

    playIO
        janela
        corFundo
        frameRate
        (Estado {imagens = imgs, modo = initialMode, acao = Parar, jogo = jogo1})
        (\estado -> desenha estado (jogador (jogo estado)) 0)
        reage
        tempo

    closeAudio