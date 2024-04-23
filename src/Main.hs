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

main :: IO ()
main = do
    imgs <- carregarImagens

    homeDir <- getEnv "HOME"

    let musicFilePath = homeDir </> "Music/aswelosesightoftheplaceweonceloungedfor.mp3"

    withAudio defaultAudio 256 $ do

        musica <- load musicFilePath

        setMusicVolume 50

        playMusic Forever musica
    
    playIO
        janela
        corFundo
        frameRate
        (Estado {imagens = imgs,modo = MenuInicial Jogar,acao = Parar,jogo = jogo1})
        (\estado -> desenha estado (jogador (jogo estado)) 0)
        reage
        tempo