-- Jogo de Damas em Haskell
-- Autores:
-- Leandro ALvares de Carvalho 202065211A
-- Vinicius ....

import Data.Char (chr, ord)

module Main where

-- Tipos que representam os jogadores, peças, casas e tabuleiro
data Jogador = A | B deriving (Eq, Show)
data Tipo = Piao | Dama deriving (Eq, Show)
data Peca = Peca Jogador Tipo deriving (Eq, Show)
data Casa = Clara | Escura (Maybe Peca) deriving (Eq, Show)

type Tabuleiro = [[Casa]]
type Pos = (Int, Int)  -- Linha, Coluna

-- Cria o tabuleiro inicial com peças do Jogador A nas 3 primeiras linhas
-- e do Jogador B nas 3 últimas, apenas nas casas escuras (linha + coluna par)
tabuleiroInicial :: Tabuleiro
tabuleiroInicial = [ [inicial l c | c <- [0..7]] | l <- [0..7] ]
  where
    inicial l c
      | (l + c) `mod` 2 == 0 = Escura (pecaInicial l)
      | otherwise            = Clara
    pecaInicial l
      | l <= 2 = Just (Peca A Piao)
      | l >= 5 = Just (Peca B Piao)
      | otherwise = Nothing

-- Converte uma casa do tabuleiro em um símbolo visível no terminal
mostrarCasa :: Casa -> String
mostrarCasa Clara = " . "
mostrarCasa (Escura Nothing) = "   "
mostrarCasa (Escura (Just (Peca A Piao))) = " a "
mostrarCasa (Escura (Just (Peca A Dama))) = " A "
mostrarCasa (Escura (Just (Peca B Piao))) = " b "
mostrarCasa (Escura (Just (Peca B Dama))) = " B "

-- Imprime o tabuleiro com letras (A–H) e números (1–8)
mostrarTabuleiro :: Tabuleiro -> String
mostrarTabuleiro tab =
  let cabecalho = "   " ++ concat [ " " ++ [chr (c + 65)] ++ " " | c <- [0..7] ]
      linhas = [ show (8 - r) ++ " " ++ concatMap mostrarCasa linha | (r, linha) <- zip [0..] (reverse tab) ]
  in unlines (cabecalho : linhas)

-- Direções válidas para movimentar um peão (frente-esquerda e frente-direita)
direcoesPiao :: Jogador -> [(Int, Int)]
direcoesPiao A = [(1, -1), (1, 1)]
direcoesPiao B = [(-1, -1), (-1, 1)]

-- Retorna a peça presente na posição informada, se houver
emJogo :: Tabuleiro -> Pos -> Maybe Peca
emJogo tab (l,c)
  | l < 0 || l > 7 || c < 0 || c > 7 = Nothing
  | otherwise = case tab !! l !! c of
                  Escura mp -> mp
                  _ -> Nothing

-- Verifica se a posição está dentro do tabuleiro
dentro :: Pos -> Bool
dentro (l,c) = l >= 0 && l <= 7 && c >= 0 && c <= 7

-- Retorna os movimentos possíveis para uma peça simples
movimentosPeca :: Tabuleiro -> Jogador -> Pos -> [Pos]
movimentosPeca tab j (l,c) =
  case emJogo tab (l,c) of
    Just (Peca _ Piao) ->
      [ (l+dl, c+dc) |
        (dl, dc) <- direcoesPiao j,
        dentro (l+dl, c+dc),
        emJogo tab (l+dl, c+dc) == Nothing ]
    _ -> []