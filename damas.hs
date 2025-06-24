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