{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Daniel Gonçalves Parente <a107363@alunos.uminho.pt>
              Tiago Silva Martins <a106938@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324
import Data.List


calculaHitbox :: Personagem -> Hitbox
calculaHitbox jogador =
    let (x,y) = posicao jogador
        (l,w) = tamanho jogador
        x1 = x - (l*0.5)
        y1 = y + (w*0.5)
        x2 = x + (l*0.5)
        y2 = y - (w*0.5)
    in ((x1,y1),(x2,y2))

calculaHitboxDano :: Personagem -> Hitbox
calculaHitboxDano jogador = if aplicaOuNaoDano && tempoRestante >0
                              then case direcao jogador of
                                      Este -> ((x1 + 1,y1),(x2 + 1,y2))
                                      Oeste -> ((x1 - 1,y1),(x2 - 1,y2))
                                      Sul -> ((x1,y1 +1 ),(x2,y2 + 1))
                                      Norte -> ((x1,y1 - 1),(x2,y2 - 1))
                              else ((x1,y1),(x2,y2))
  where
      (x,y) = posicao jogador
      (c,l) = tamanho jogador
      (aplicaOuNaoDano,tempoRestante) = aplicaDano jogador
      ((x1,y1),(x2,y2)) = calculaHitbox jogador

interHitboxes :: Hitbox -> Hitbox -> Bool
interHitboxes hitbox1 hitbox2 = x3 >= (x1 - 1) && x3 <= x2 && y4 >= (y2 - 1) && y4 <= (y1 + 1)
    where
        ((x1,y1),(x2,y2)) = hitbox1
        ((x3,y3),(x4,y4)) = hitbox2

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens jogador inimigo = interHitboxes hitboxJogador hitboxInimigo
    where
        hitboxJogador = calculaHitbox jogador
        hitboxInimigo = calculaHitbox inimigo

indicesBlocosMapa :: Mapa -> [((Int,Int),Bloco)]
indicesBlocosMapa (Mapa _ _ matriz) = concatMapaIndices (\i row -> mapaIndices (\j bloco -> ((i,j),bloco)) row) matriz
    where
        mapaIndices :: (Int -> a -> b) -> [a] -> [b]
        mapaIndices f = zipWith f [0..]

        concatMapaIndices :: (Int -> a -> [b]) -> [a] -> [b]
        concatMapaIndices f = concat . zipWith f [0..]

filtraBlocosPlataformaAlcapao :: [((Int,Int),Bloco)] -> [((Int,Int),Bloco)]
filtraBlocosPlataformaAlcapao = filter (\(_,bloco) -> bloco == Plataforma || bloco == Alcapao)

filtraBlocosPlataforma :: [((Int,Int),Bloco)] -> [((Int,Int),Bloco)]
filtraBlocosPlataforma = filter (\(_,bloco) -> bloco == Plataforma)

filtraBlocosAlcapao :: [((Int,Int),Bloco)] -> [((Int,Int),Bloco)]
filtraBlocosAlcapao = filter (\(_,bloco) -> bloco == Alcapao)

filtraBlocosEscada :: [((Int,Int),Bloco)] -> [((Int,Int),Bloco)]
filtraBlocosEscada = filter (\(_,bloco) -> bloco == Escada)

devolveIndices :: [((Int,Int),Bloco)] -> [(Int,Int)]
devolveIndices = map (\((a,b),_) -> (a,b))

converteIndiceParaPosicao :: (Int,Int) -> Posicao
converteIndiceParaPosicao (i,j) = (fromIntegral j,fromIntegral i)

convertePosicaoParaIndice :: Posicao -> (Int,Int)
convertePosicaoParaIndice (x,y) = (floor y,floor x)

colisaoComLimitesMapa :: Mapa -> Personagem -> Bool
colisaoComLimitesMapa mapa@(Mapa _ _ lblocos) jogador = foraDoMapa mapa jogador || colideComLimites mapa jogador

foraDoMapa :: Mapa -> Personagem -> Bool
foraDoMapa mapa@(Mapa _ _ lblocos) jogador = x < 0.5 || x > fromIntegral (length (head lblocos)) - 0.5 || y < 0.5 || y > fromIntegral (length lblocos) - 0.5
    where
        (x,y) = posicao jogador

colideComLimites :: Mapa -> Personagem -> Bool
colideComLimites mapa@(Mapa _ _ lblocos) jogador =
    (x1 >= 0 && x1 <= fromIntegral (length (head lblocos)) && y1 == 1)
    || (y1 == fromIntegral (length lblocos) && x1 >= 0 && x1 <= fromIntegral (length (head lblocos)) - 1)
    || (x1 == 0 && y1 >= 1 && y1 <= fromIntegral (length lblocos))
    || (x1 == fromIntegral (length (head lblocos)) - 1 && y1 >= 1 && y1 <= fromIntegral (length lblocos))
    where
        (x,y) = posicao jogador
        ((x1,y1),(x2,y2)) = calculaHitbox jogador

colisaoComBlocos :: Mapa -> Personagem -> Bool
colisaoComBlocos mapa@(Mapa _ _ lblocos) jogador = -- any (verificaColisaoBlocos (calculaHitbox jogador)) indicesBlocosFiltrados || 
    any (verificaColisao (calculaHitbox jogador)) indicesBlocosFiltrados
    where
        indicesBlocosFiltrados = devolveIndices $ filtraBlocosPlataformaAlcapao $ indicesBlocosMapa mapa

colisaoComBlocos' :: [(Int,Int)] -> Personagem -> Bool
colisaoComBlocos' mapa jogador = -- any (verificaColisaoBlocos (calculaHitbox jogador)) indicesBlocosFiltrados || 
    any (verificaColisao (calculaHitbox jogador)) mapa

verificaColisao :: Hitbox -> (Int,Int) -> Bool
verificaColisao hitbox@((x1,y1),(x2,y2)) (i,j) =
    (y2 == (y3 + 1) && x1 >= (x3 - 1) && x1 <= (x3 + 1))
    || (y2 == (y3 + 1) && x2 >= x3 && x2 <= (x3 + 1))
    || (y1 >= y3 && y1 <= (y3 + 1) && x1 >= (x3 - 0.9) && x1 <= x3)
    || (y1 <= (y3 + 1.5) && y1 >= y3 && x1 <=(x3 + 1) && x1 >= x3)
  where
    (x3,y3) = converteIndiceParaPosicao (i,j)

verificaColisaoS :: Hitbox -> (Int,Int) -> Bool
verificaColisaoS hitbox@((x1,y1),(x2,y2)) (i,j) = y1 == y3 && x1 > (x3 - 0.5) && x1 < (x3 + 0.5)
  where (x3,y3) = converteIndiceParaPosicao (i,j)

colisaoComPlataforma :: Mapa -> Personagem -> Bool
colisaoComPlataforma mapa@(Mapa _ _ lblocos) jogador = -- any (verificaColisaoBlocos (calculaHitbox jogador)) indicesBlocosFiltrados ||
    any (verificaColisao (calculaHitbox jogador)) indicesBlocosFiltrados
    where
        indicesBlocosFiltrados = devolveIndices $ filtraBlocosPlataforma $ indicesBlocosMapa mapa

colisaoComEscada :: Mapa -> Personagem -> Bool
colisaoComEscada mapa@(Mapa _ _ lblocos) jogador = -- any (verificaColisaoBlocos (calculaHitbox jogador)) indicesBlocosFiltrados ||
    any (verificaColisao (calculaHitbox jogador)) indicesBlocosFiltrados
    where
        indicesBlocosFiltrados = devolveIndices $ filtraBlocosEscada $ indicesBlocosMapa mapa

colisaoComAlcapao :: Jogo -> Mapa
colisaoComAlcapao jogo@(Jogo mapa@(Mapa pos jogadorDir matriz) i c jogador)
  | colisaoComBlocos mapa jogador && any (verificaColisaoS (calculaHitbox jogador)) indicesBlocosAlcapao =
    Mapa pos jogadorDir (atualizaMatrizAlcapao matriz indiceAlcapao)
  | otherwise = Mapa pos jogadorDir matriz
  where
    indicesBlocosAlcapao = devolveIndices $ filtraAlcapao $ indicesBlocosMapa mapa

    -- filtra apenas os blocos com alçapões
    filtraAlcapao :: [((Int,Int),Bloco)] -> [((Int,Int),Bloco)]
    filtraAlcapao = filter (\(_,bloco) -> bloco == Alcapao)

    -- retorna o par do índice do bloco do alçapão que foi pisado
    indiceAlcapao :: (Int,Int)
    indiceAlcapao = head $ filter (verificaColisaoS (calculaHitbox jogador)) indicesBlocosAlcapao

    -- atualiza o índice do bloco atualizado na matriz dos blocos do mapa
    atualizaMatrizAlcapao :: [[Bloco]] -> (Int,Int) -> [[Bloco]]
    atualizaMatrizAlcapao mat (i, j) = map (atualizaLinhaAlcapao (i, j)) $ zip [0..] mat

    -- atualiza o índice do bloco na linha da matriz dos blocos do mapa
    atualizaLinhaAlcapao :: (Int,Int) -> (Int,[Bloco]) -> [Bloco]
    atualizaLinhaAlcapao (i, j) (k, linha) = map (atualizaBlocoAlcapao (i, j) k) $ zip [0..] linha

    -- recebe o índice do bloco que precisa de ser atualizado (e fá-lo, se necessário)
    atualizaBlocoAlcapao :: (Int,Int) -> Int -> (Int,Bloco) -> Bloco
    atualizaBlocoAlcapao (i,j) k (l,bloco) =
                if (k,l) == (i,j)
                  then Vazio
                  else bloco

-- vê quais são as plataformas que se encontram imediatamente acima de uma escada
blocosSobreEscada :: Mapa -> [(Int,Int)]
blocosSobreEscada mapa = blocosSobreEscada' indicesBlocosFiltrados indicesBlocosFiltrados1
  where  indicesBlocosFiltrados = devolveIndices $ filtraBlocosPlataformaAlcapao $ indicesBlocosMapa mapa
         indicesBlocosFiltrados1 = devolveIndices $ filtraBlocosEscada $ indicesBlocosMapa mapa

blocosSobreEscada' :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
blocosSobreEscada' _ [] = []
blocosSobreEscada' [] _ = []
blocosSobreEscada' ((p,r):ps) e = blocosSobreEscada'' (p,r) e ++ blocosSobreEscada' ps e

blocosSobreEscada'' :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
blocosSobreEscada'' _ [] = []
blocosSobreEscada'' (p,r) e
               | (p+1,r) `elem` e = [(p,r)]
               | otherwise = []

verificaColisaoMoeda :: Jogo -> Jogo
verificaColisaoMoeda jogo@(Jogo mapa inimigos colecionaveis jogador) =
    let hitboxJogador = calculaHitbox jogador
        colecionaveisRestantes = filter (\(colec,pos) -> not (interHitboxes hitboxJogador (pos,pos))) colecionaveis
        moedasColididas = filter (\(colec,pos) -> colec == Moeda && interHitboxes hitboxJogador (pos,pos)) colecionaveis
        novaPontuacao = pontuacao jogador + 10*length moedasColididas
        novoJogador = jogador {pontuacao = novaPontuacao}
    in jogo {jogador = novoJogador, colecionaveis = colecionaveisRestantes}

verificaColisaoMartelo :: Double -> Jogo -> Jogo
verificaColisaoMartelo dt jogo@(Jogo mapa inimigos colecionaveis jogador) =
    let hitboxJogador = calculaHitbox jogador
        (aplicaOuNaoDano,tempoRestante) = aplicaDano jogador
        martelosColididos = filter (\(colec, pos) -> colec == Martelo && interHitboxes hitboxJogador (pos,pos)) colecionaveis
        novoTempoMartelo = max 0 (tempoRestante - dt)
        novoAplicaDano = if not (null martelosColididos) then jogador {aplicaDano = (True, 10)} else jogador
    in jogo {jogador = novoAplicaDano}

colideComEstrela :: Jogo -> Bool
colideComEstrela jogo@(Jogo mapa@(Mapa(pos,dir) (x,y) matriz) inimigos colecionaveis jogador) = interHitboxes (calculaHitbox jogador) (calculaHitboxEstrela (x,y))
 where
     calculaHitboxEstrela :: Posicao -> Hitbox
     calculaHitboxEstrela (x,y) = ((x-1/2,y+1/2),(x+1/2,y-1/2))

colideComDK :: Jogo -> Bool
colideComDK jogo@(Jogo mapa@(Mapa (pos,dir) (x,y) matriz) inimigos colecionaveis jogador) = undefined

ficasemvidas :: Jogo -> Bool
ficasemvidas e@Jogo {mapa = mapaAtual, jogador = mario}
         | vida mario == 0 = True
         | otherwise = False



verificaColisoes :: Jogo -> Jogo
verificaColisoes jogo@(Jogo _ inimigos _ jogador) =
      case aplicaDano jogador of
          (True,_) -> verificaColisoesComMartelo jogo
          _ -> verificaColisoesSemMartelo jogo

verificaColisoesComMartelo :: Jogo -> Jogo
verificaColisoesComMartelo jogo@(Jogo _ inimigos _ jogador) = foldr colideComInimigoComMartelo jogo inimigos

colideComInimigoComMartelo :: Personagem -> Jogo -> Jogo
colideComInimigoComMartelo inimigo jogo@(Jogo mapa inimigos colecionaveis jogador) =
  if interHitboxes (calculaHitboxDano jogador) (calculaHitbox inimigo)
    then Jogo mapa (atualizaInimigoComMartelo inimigo : inimigos) colecionaveis jogador
    else jogo

atualizaInimigoComMartelo :: Personagem -> Personagem
atualizaInimigoComMartelo inimigo =
  if vida inimigo <= 0
    then inimigo {posicao = (200,200)}
    else inimigo {vida = vida inimigo - 1}

verificaColisoesSemMartelo :: Jogo -> Jogo
verificaColisoesSemMartelo jogo@(Jogo _ inimigos colecionaveis jogador) = foldr (colideComInimigoSemMartelo jogador) jogo inimigos

colideComInimigoSemMartelo :: Personagem -> Personagem -> Jogo -> Jogo
colideComInimigoSemMartelo jogador inimigo jogo@(Jogo mapa inimigos colecionaveis _) =
  if interHitboxes (calculaHitbox jogador) (calculaHitbox inimigo)
    then Jogo mapa (atualizaJogadorSemMartelo jogador inimigo : filter (/= inimigo) inimigos) colecionaveis jogador
    else jogo

atualizaJogadorSemMartelo :: Personagem -> Personagem -> Personagem
atualizaJogadorSemMartelo jogador inimigo =
  if vida inimigo > 0
    then jogador {vida = vida jogador - 1, aplicaDano = (False,0)}
    else jogador