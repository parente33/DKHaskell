module Tarefa5 where

import Data.Maybe (fromJust)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitFailure)
import LI12324
import Tarefa1
import Tarefa3
import Tarefa4
import GHC.Base (TrName(TrNameD))
import GHC.Conc (ThreadStatus(ThreadRunning))
import System.FilePath ((</>))
import SDL.Mixer

--Recolhe a Picture que esta associada a uma imagem
getImagem :: Imagem -> Imagens -> Picture
getImagem k d = fromJust $ lookup k d

desenha :: Estado -> Personagem -> Float -> IO Picture
desenha e@Estado {modo = MenuInicial Jogar, imagens = imgs} _ _ = do
  return $ Pictures [getImagem MenuBackgroundImage imgs, Translate 470 (-250) $ getImagem PlayButton2 imgs, Translate 470 (-400) $ getImagem Quit imgs]
desenha e@Estado {modo = MenuInicial Sair, imagens = imgs} _ _ = do
  return $ Pictures [getImagem MenuBackgroundImage imgs, Translate 470 (-250) $ getImagem PlayButton imgs, Translate 470 (-400) $ getImagem Quit2 imgs]

desenha e@Estado {modo = MenuPausa Retornar, imagens = imgs} _ _ = do
  return $ Pictures [getImagem Menudepausa imgs, Translate 470 (-250) $ getImagem Retornarbutton imgs, Translate 470 (-400) $ getImagem Exitbutton2 imgs]
desenha e@Estado {modo = MenuPausa Exit, imagens = imgs} _ _ = do
  return $ Pictures [getImagem Menudepausa imgs, Translate 470 (-250) $ getImagem Retornarbutton2 imgs, Translate 470 (-400) $ getImagem Exitbutton imgs]

desenha e@Estado {modo = EmJogo, imagens = imgs, jogo = jogo, acao = acaoAtual} jogador t = do
    let imagemJogador = imagemMario imgs e t jogador
    return $ Pictures (getImagem GameBackgroundImage imgs : mapaImagem (colisaoComAlcapao jogo) imgs 1 
        ++ [atualizaEstadoJogador e] 
        ++ posicaoEstrela imgs jogo 
        ++ posicaoColecionaveis imgs jogo 
        ++ desenhaVidas imgs jogo 
        ++ desenhaPontuacao imgs e 
        ++ posicaoFantasmas imgs jogo 
        ++ [posicaoMacaco imgs donkeyKong] 
        ++ [imagemJogador] 
        ++ desenhaVitoriaOuDerrota imgs jogo)


desenhaVidas :: Imagens -> Jogo -> [Picture]
desenhaVidas imgs Jogo {jogador = mario}
           | n == 3 = Translate (-320) 460 (getImagem Coracao imgs) : Translate (-270) 460 (getImagem Coracao imgs) : [Translate (-220) 460 (getImagem Coracao imgs)]
           | n == 2 = Translate (-320) 460 (getImagem Coracao imgs) : Translate (-270) 460 (getImagem Coracao imgs) : [Translate (-220) 460 (getImagem Coracaopartido imgs)]
           | n == 1 = Translate (-320) 460 (getImagem Coracao imgs) : Translate (-270) 460 (getImagem Coracaopartido imgs) : [Translate (-220) 460 (getImagem Coracaopartido imgs)]
           | n == 0 = Translate (-320) 460 (getImagem Coracaopartido imgs) : Translate (-270) 460 (getImagem Coracaopartido imgs) : [Translate (-220) 460 (getImagem Coracaopartido imgs)]
           | otherwise = []
              where n = vida mario

desenhaPontuacao :: Imagens -> Estado -> [Picture]
desenhaPontuacao imgs e@Estado {jogo = Jogo _ _ _ jogador} = [Color red (Translate 230 450 $ Scale 0.20 0.20 $ Text ("Score:"++show n))]
    where n = pontuacao jogador

--Calcula a posicao da estrela no ecra
posicaoEstrela :: Imagens -> Jogo -> [Picture]
posicaoEstrela imgs Jogo {mapa = (Mapa _ (x,y) _)} = [Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) (getImagem EstrelaBMP imgs)]

desenhaVitoriaOuDerrota :: Imagens -> Jogo -> [Picture]
desenhaVitoriaOuDerrota imgs j
           | colideComEstrela j = [getImagem Vitoria imgs]
           | ficasemvidas j = [getImagem Derrota imgs]
           | otherwise = []

 --Calcula a pocicao dos colecionaveis no ecra
posicaoColecionaveis :: Imagens -> Jogo -> [Picture]
posicaoColecionaveis imgs Jogo {colecionaveis = col} = posicaoColecionaveis' imgs col

 --funcao auxiliar à anterior
posicaoColecionaveis' :: Imagens -> [(Colecionavel, Posicao)] -> [Picture]
posicaoColecionaveis' _ [] = []
posicaoColecionaveis' imgs ((c,(x,y)):xs)
           | c == Moeda = Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) (getImagem MoedaBMP imgs) : posicaoColecionaveis' imgs xs
           | c == Martelo = Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) (getImagem MarteloBMP imgs) : posicaoColecionaveis' imgs xs
           | c == KitMedico = Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) (getImagem MedicKit imgs) : posicaoColecionaveis' imgs xs
           | otherwise = []

imagemMario :: Imagens -> Estado -> Float -> Personagem -> Picture
imagemMario imgs e@Estado {jogo = jogo@(Jogo {jogador = mario}), acao = acaoAtual} t _ =
    let (x, y) = posicao mario
        (vx,vy) = velocidade mario
        z = direcao mario
        (aplicaOuNaoDano,tempoRestante) = aplicaDano mario
        imagemParado = getImagem (direcaodaimagem z) imgs
        imagemAndandoEsquerda = getImagem Mariorunning imgs
        imagemAndandoDireita = getImagem Mariorunningdireita imgs
        imagemSubindoEscada = getImagem Marioescada imgs
        imagemSubindoEscadaDireita = getImagem Marioescadadireita imgs
        imagemDescendoEscada = getImagem Marioescada imgs
        imagemDescendoEscadaDireita = getImagem Marioescadadireita imgs
        imagemSaltando = getImagem Mariojumping imgs
        imagemSaltandoDireita = getImagem Mariojumpingdireita imgs
        imagemMartelando = getImagem Mariohammer imgs
        imagemAndandoMartelando = getImagem Mariohammerunning imgs
        imagemMartelandoDireita = getImagem Mariohammerdireita imgs
        imagemAndandoMartelandoDireita = getImagem Mariohammerrunningdireita imgs
        imagemMarteloFazBong = getImagem Mariohammer2 imgs
        imagemMarteloFazBongDireita = getImagem Mariohammer2direita imgs
        trocaImagem = mod (round t) 2
      in
          if aplicaOuNaoDano && tempoRestante > 0
            then
                if z == Oeste
                  then
                      if vx == 0
                        then
                            if trocaImagem == 0
                              then Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20.3)*(-24)) imagemMartelando
                              else Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemMarteloFazBong
                        else
                            if trocaImagem == 0
                              then Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20.3)*(-24)) imagemAndandoMartelando
                              else Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemMarteloFazBong
                  else
                      if vx == 0
                        then
                            if trocaImagem == 0
                              then Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20.3)*(-24)) imagemMartelandoDireita
                              else Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemMarteloFazBongDireita
                        else
                            if trocaImagem == 0
                              then Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20.3)*(-24)) imagemAndandoMartelandoDireita
                              else Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemMarteloFazBongDireita
            else
                if vx < 0
                  then
                      if trocaImagem == 0
                        then Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemAndandoEsquerda
                        else Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemParado
                  else
                      if vx > 0 -- vx tem que ser 0 para jogador subir escada?
                        then
                            if trocaImagem == 0
                              then Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemAndandoDireita
                              else
                                  if vy < 0
                                    then
                                        if emEscada mario
                                          then
                                              if trocaImagem == 0
                                                then Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemSubindoEscada
                                                else Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemSubindoEscadaDireita
                                          else
                                              if z == Oeste
                                                then Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemSaltando
                                                else Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemSaltandoDireita
                                    else
                                        if emEscada mario
                                          then
                                              if trocaImagem == 0
                                                then Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemDescendoEscada
                                                else Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemDescendoEscadaDireita
                                          else Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemParado
                        else Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) imagemParado

posicaoFantasmas :: Imagens -> Jogo -> [Picture]
posicaoFantasmas imgs Jogo {inimigos = inimigos} = posicaoFantasmas' imgs inimigos

posicaoFantasmas' :: Imagens -> [Personagem] -> [Picture]
posicaoFantasmas' _ [] = []
posicaoFantasmas' imgs (x:xs) = posicaoFantasmas'' imgs x : posicaoFantasmas' imgs xs

posicaoFantasmas'' :: Imagens -> Personagem -> Picture
posicaoFantasmas'' imgs inimigo = Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) $ getImagem (direcaodaimagemInimigo z) imgs
  where (x,y) = posicao inimigo
        z = direcao inimigo

 --Calcula a posicao do Mario no ecra
posicaoMacaco :: Imagens -> Personagem -> Picture
posicaoMacaco imgs macaco = Translate (realToFrac (14.84-x)*(-24)) (realToFrac (y-20)*(-24)) $ getImagem Macaco imgs
  where (x,y) = posicao macaco

--Escolhe o BMP de acordo com a direcao
direcaodaimagem :: Direcao -> Imagem
direcaodaimagem x
          | x == Este = Mariodireita
          | x == Oeste = Mario
          | x == Norte = Marioescada
          | x == Sul = Marioescadadireita

--Escolhe o BMP de acordo com a direcao
direcaodaimagemInimigo :: Direcao -> Imagem
direcaodaimagemInimigo x
          | x == Este = FantasmaDireitaBMP
          | x == Oeste = FantasmaBMP

--Tranforma um bloco no seu respetivo BMP
blocotobmp :: [Bloco] -> Imagem
blocotobmp [] = EstrelaBMP
blocotobmp (x:xs)
          | x == Escada = EscadasBMP
          | x == Plataforma = PlataformaBMP
          | x == Alcapao = AlcapaoBMP
          | otherwise = EmptyBMP

--Calcula a posicao de um bloco no ecra
blocotoimagem :: Bloco -> Imagens -> Float -> Float -> Picture
blocotoimagem x imgs n l = Translate (-320+24*(l-2)) (468-24*(n-1)) $ getImagem (blocotobmp [x]) imgs

--Calcula a posicao de todos os blocos do mapa no ecra
mapaImagem :: Mapa -> Imagens -> Float -> [Picture]
mapaImagem (Mapa z y []) _ _ = []
mapaImagem (Mapa z y (x:xs)) imgs n = linhatoimagem x imgs n 1 ++ mapaImagem (Mapa z y xs) imgs (n+1)

--Calcula a posicao de todos os blocos de uma linha do mapa no ecra
linhatoimagem :: [Bloco] -> Imagens -> Float -> Float -> [Picture]
linhatoimagem [] _ _ _ = []
linhatoimagem (x:xs) imgs n z = blocotoimagem x imgs n z : linhatoimagem xs imgs n (z+1)

--Inputs das teclas e os seus respetivos Outputs
reage :: Event -> Estado -> IO Estado
reage (EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial Jogar} =
  return e {modo = MenuInicial Sair}
reage (EventKey (Char 's') Down _ _) e@Estado {modo = MenuInicial Jogar} =
  return e {modo = MenuInicial Sair}
reage (EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial Sair} =
  return e {modo = MenuInicial Jogar}
reage (EventKey (Char 'w') Down _ _) e@Estado {modo = MenuInicial Sair} =
  return e {modo = MenuInicial Jogar}
reage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Jogar} = do
  return e {modo = EmJogo}
reage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Sair} =
  exitFailure
reage (EventKey (SpecialKey KeyLeft) Down _ _) e@Estado {modo = EmJogo,jogo = jogo1} =
  return $ atualizaestado (Just AndarEsquerda) e
reage (EventKey (SpecialKey KeyLeft) Up _ _) e@Estado {modo= EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just Parar) e
reage (EventKey (Char 'a') Down _ _) e@Estado {modo = EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just AndarEsquerda) e
reage (EventKey (Char 'a') Up _ _) e@Estado {modo = EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just Parar) e
reage (EventKey (SpecialKey KeyRight) Down _ _) e@Estado {modo = EmJogo,jogo = jogo1} =
  return $ atualizaestado (Just AndarDireita) e
reage (EventKey (SpecialKey KeyRight) Up _ _) e@Estado {modo= EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just Parar) e
reage (EventKey (Char 'd') Down _ _) e@Estado {modo = EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just AndarDireita) e
reage (EventKey (Char 'd') Up _ _) e@Estado {modo = EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just Parar) e
reage (EventKey (SpecialKey KeySpace) Down _ _) e@Estado {modo = EmJogo,jogo = jogo1} =
  return $ atualizaestado (Just Saltar) e
reage (EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo= EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just Subir) e
reage (EventKey (SpecialKey KeyUp) Up _ _) e@Estado{modo= EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just Parar) e
reage (EventKey (Char 'w') Down _ _) e@Estado {modo = EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just Subir) e
reage (EventKey (Char 'w') Up _ _) e@Estado {modo = EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just Parar) e
reage (EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo= EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just Descer) e
reage (EventKey (SpecialKey KeyDown) Up _ _) e@Estado {modo= EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just Parar) e
reage (EventKey (Char 's') Down _ _) e@Estado {modo = EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just Descer) e
reage (EventKey (Char 's') Up _ _) e@Estado {modo = EmJogo,jogo = jogo1} = do
  return $ atualizaestado (Just Parar) e
reage (EventKey (SpecialKey KeyEsc) Down _ _) e@Estado {modo = EmJogo,jogo = jogo1} =
  return e {modo = MenuPausa Retornar}
reage (EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuPausa Retornar} =
  return e {modo = MenuPausa Exit}
reage (EventKey (Char 's') Down _ _) e@Estado {modo = MenuPausa Retornar} =
  return e {modo = MenuPausa Exit}
reage (EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuPausa Exit} =
  return e {modo = MenuPausa Retornar}
reage (EventKey (Char 'w') Down _ _) e@Estado {modo = MenuPausa Exit} =
  return e {modo = MenuPausa Retornar}
reage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuPausa Exit} =
  return reiniciarJogo $ e {modo = MenuInicial Jogar}
reage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuPausa Retornar} =
  return e {modo = EmJogo}
-- para teste
reage (EventKey (SpecialKey KeyEsc) Down _ _) e@Estado {modo = MenuPausa Retomar} =
  return e {modo = EmJogo}
reage (EventKey (SpecialKey KeyEsc) Down _ _) e@Estado {modo = MenuPausa Exit} =
  return e {modo = EmJogo}
reage _ e = return e

pararMusicaAtual :: IO ()
pararMusicaAtual = SDL.Mixer.haltMusic

janela :: Display
janela = FullScreen

corFundo :: Color
corFundo = greyN 0.0

frameRate :: Int
frameRate = 500

atualizaEstado :: Float -> Estado -> Estado
atualizaEstado dt e =
  let jogoAtualizado = atualizaPosicaoMario dt (jogo e)
  in e {jogo = jogoAtualizado}

-- Só para debugging
atualizaEstadoJogador :: Estado -> Picture
atualizaEstadoJogador estado = let  jogoAtualizado = atualizaEstado 2024 estado
                                    jogadorAtualizado = jogador (jogo jogoAtualizado)

                                    txtVel = "Velocidade = " ++ show (velocidade jogadorAtualizado)
                                    imgVel = translate 370 450 $ scale 0.2 0.2 $ color black $ text txtVel
                                    imgVelBold = translate 369 449 $ scale 0.2 0.2 $ color black $ text txtVel

                                    txtTipo = "Tipo = " ++ show (tipo jogadorAtualizado)
                                    imgTipo = translate 370 425 $ scale 0.2 0.2 $ color black $ text txtTipo
                                    imgTipoBold = translate 369 424 $ scale 0.2 0.2 $ color black $ text txtTipo

                                    txtPos = "Posicao = " ++ show (posicao jogadorAtualizado)
                                    imgPos = translate 370 405 $ scale 0.2 0.2 $ color black $ text txtPos
                                    imgPosBold = translate 369 404 $ scale 0.2 0.2 $ color black $ text txtPos

                                    txtDir = "Direcao = " ++ show (direcao jogadorAtualizado)
                                    imgDir = translate 370 385 $ scale 0.2 0.2 $ color black $ text txtDir
                                    imgDirBold = translate 369 384 $ scale 0.2 0.2 $ color black $ text txtDir

                                    txtTam = "Tamanho = " ++ show (tamanho jogadorAtualizado)
                                    imgTam = translate 370 365 $ scale 0.2 0.2 $ color black $ text txtTam
                                    imgTamBold = translate 369 364 $ scale 0.2 0.2 $ color black $ text txtTam

                                    txtEsc = "EmEscada = " ++ show (emEscada jogadorAtualizado)
                                    imgEsc = translate 370 345 $ scale 0.2 0.2 $ color black $ text txtEsc
                                    imgEscBold = translate 369 344 $ scale 0.2 0.2 $ color black $ text txtEsc

                                    txtRes = "Ressalta = " ++ show (ressalta jogadorAtualizado)
                                    imgRes = translate 370 325 $ scale 0.2 0.2 $ color black $ text txtRes
                                    imgResBold = translate 369 324 $ scale 0.2 0.2 $ color black $ text txtRes

                                    txtVida = "Vida = " ++ show (vida jogadorAtualizado)
                                    imgVida = translate 370 305 $ scale 0.2 0.2 $ color black $ text txtVida
                                    imgVidaBold = translate 369 304 $ scale 0.2 0.2 $ color black $ text txtVida

                                    txtPont = "Pontuacao = " ++ show (pontuacao jogadorAtualizado)
                                    imgPont = translate 370 285 $ scale 0.2 0.2 $ color black $ text txtPont
                                    imgPontBold = translate 369 284 $ scale 0.2 0.2 $ color black $ text txtPont

                                    txtDano = "AplicaDano = " ++ show (aplicaDano jogadorAtualizado)
                                    imgDano = translate 370 265 $ scale 0.2 0.2 $ color black $ text txtDano
                                    imgDanoBold = translate 369 264 $ scale 0.2 0.2 $ color black $ text txtDano

                                in pictures [imgVel,imgVelBold,imgTipo,imgTipoBold,imgPos,imgPosBold,imgDir,imgDirBold,imgTam,imgTamBold,imgEsc,imgEscBold,
                                 imgRes,imgResBold,imgVida,imgVidaBold,imgPont,imgPontBold,imgDano,imgDanoBold]

tempo :: Float -> Estado -> IO Estado
tempo dt estadoAtual@(Estado {modo = modoAtual, imagens = imgs})
  | modoAtual /= EmJogo = return estadoAtual
  | otherwise = do
      putStrLn $ "Before Update: " ++ show (posicao (jogador (jogo estadoAtual)))

      let estadoAtualizado = atualizaEstado dt estadoAtual
          jogoMovimentado = movimenta 2024 (realToFrac dt) (jogo estadoAtualizado)
          jogadorAtualizado = jogador jogoMovimentado
          (aplicaDanoMartelo,tempoMartelo) = aplicaDano jogadorAtualizado

      let tempoMarteloAtualizado = if aplicaDanoMartelo && tempoMartelo > 0
                                      then max 0 (tempoMartelo - realToFrac dt)
                                      else 0
          aplicaDanoFinal = if tempoMarteloAtualizado <= 0 then (False,0) else (aplicaDanoMartelo,tempoMarteloAtualizado)

      let jogadorAtualizadoComMartelo = jogadorAtualizado {aplicaDano = aplicaDanoFinal}
          imagemJogadorAtualizada = imagemMario imgs estadoAtual dt

      let jogoFinal = jogoMovimentado {jogador = jogadorAtualizadoComMartelo}

      estadoDesenhado <- desenha estadoAtualizado jogadorAtualizadoComMartelo (realToFrac dt)

      return estadoAtualizado {jogo = jogoFinal}

reiniciarJogo :: IO Estado
reiniciarJogo = do
    imgs <- carregarImagens
    return Estado {
        jogo = jogo1,
        imagens = imgs,
        modo = MenuInicial Jogar,
        acao = Parar}

--Carrega todos os BMP
carregarImagens :: IO Imagens
carregarImagens = do
  alcapao <-loadBMP $ "assets" </> "alcapao.bmp"
  coracaopartido <- loadBMP $ "assets" </> "coracaopartido.bmp"
  coracao <- loadBMP$ "assets" </> "coracao.bmp"
  escadas <- loadBMP $ "assets" </> "escadas.bmp"
  estrela <- loadBMP $ "assets" </> "estrela.bmp"
  hammer <- loadBMP $ "assets" </> "hammer.bmp"
  mariohammerbonk <- loadBMP $ "assets" </> "mario_bonk_standing.bmp"
  mariohammerbonkdireita <- loadBMP $ "assets" </> "mario_bonk_standing_direita.bmp"
  mariodead <- loadBMP $ "assets" </> "mario_dead.bmp"
  marioescada <- loadBMP $ "assets" </> "mario_escada.bmp"
  marioescadadireita <- loadBMP $ "assets" </> "mario_escada_direita.bmp"
  mariohammerunning <- loadBMP $ "assets" </> "mario_hammer_running.bmp"
  mariohammerrunningdireita <- loadBMP $ "assets" </> "mario_hammer_running_direita.bmp"
  mariohammer <- loadBMP $ "assets" </> "mario_hammer_standing.bmp"
  mariohammerdireita <- loadBMP $ "assets" </> "mario_hammer_standing_direita.bmp"
  mariojumping <- loadBMP $ "assets" </> "mario_jumping.bmp"
  mariojumpingdireita <- loadBMP $ "assets" </> "mario_jumping_direita.bmp"
  mariorunning <- loadBMP $ "assets" </> "mario_running.bmp"
  mariorunningdireita <- loadBMP $ "assets" </> "mario_running_direita.bmp"
  mario <- loadBMP $ "assets" </> "mario_standing.bmp"
  mariodireita <- loadBMP $ "assets" </> "mario_standing_direita.bmp"
  macaco <- loadBMP $ "assets" </> "monke.bmp"
  plataforma <- loadBMP $ "assets" </> "plataforma.bmp"
  princesa <- loadBMP $ "assets" </> "princesa.bmp"
  playButton <- loadBMP $ "assets" </> "play_button.bmp"
  playButton2 <- loadBMP $ "assets" </> "amarelo.bmp"
  quitButton <- loadBMP $ "assets" </> "quit.bmp"
  quitButton2 <- loadBMP $ "assets" </> "amarelo2.bmp"
  menuBackground <- loadBMP $ "assets" </> "Background_direito.bmp"
  gameBackground <- loadBMP $ "assets" </> "game_background.bmp"
  empty <- loadBMP $ "assets" </> "empty.bmp"
  moeda <- loadBMP $ "assets" </> "moeda.bmp"
  fantasma <- loadBMP $ "assets" </> "fantasma.bmp"
  fantasmaDireita <- loadBMP $ "assets" </> "fantasmaDireita.bmp"
  vitoria <- loadBMP $ "assets" </> "vitoria.bmp"
  derrota <- loadBMP $ "assets" </> "derrota.bmp"
  menuPausa <- loadBMP $ "assets" </> "pause_menu.bmp"
  returnButton <- loadBMP $ "assets" </> "retornar_button.bmp"
  exitButton <- loadBMP $ "assets" </> "exit_button.bmp"
  returnButton2 <- loadBMP $ "assets" </> "retornar_button2.bmp"
  exitButton2 <- loadBMP $ "assets" </> "exit_button2.bmp"
  medickit <- loadBMP $ "assets" </> "medic_kit.bmp"
  let imgs = [(Derrota,derrota),(Vitoria,vitoria),(FantasmaDireitaBMP,fantasmaDireita),(FantasmaBMP,fantasma),(MoedaBMP,moeda),(EmptyBMP,empty),
              (Princesa,princesa),(PlataformaBMP,plataforma),(Macaco,macaco),(Mariodireita,mariodireita),(Mario,mario),(Mariorunningdireita,mariorunningdireita),
              (Mariorunning,mariorunning),(Mariojumpingdireita,mariojumpingdireita),(Mariojumping,mariojumping),(Mariohammerdireita,mariohammerdireita),
              (Mariohammer,mariohammer),(Mariohammerrunningdireita,mariohammerrunningdireita),(Mariohammerunning,mariohammerunning),
              (Marioescadadireita,marioescadadireita),(Marioescada,marioescada),(Mariodead,mariodead),(Mariohammer2direita,mariohammerbonkdireita),
              (Mariohammer2,mariohammerbonk),(MarteloBMP,hammer),(EstrelaBMP,estrela),(EscadasBMP,escadas),(Coracao,coracao),(Coracaopartido,coracaopartido),
              (AlcapaoBMP,alcapao),(PlayButton, playButton),(MenuBackgroundImage,menuBackground),(GameBackgroundImage,gameBackground),
              (PlayButton2,playButton2),(Quit,quitButton),(Quit2,quitButton2),(Exitbutton,exitButton),(Retornarbutton,returnButton),(Menudepausa,menuPausa),
              (Retornarbutton2,returnButton2),(Exitbutton2,exitButton2),(MedicKit,medickit)]
  return imgs