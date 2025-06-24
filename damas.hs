-- Jogo de Damas em Haskell
-- Autores:
-- Leandro ALvares de Carvalho 202065211A
-- Vinicius ....

module Main where

-- Tipos que representam os jogadores, peças, casas e tabuleiro
data Jogador = A | B deriving (Eq, Show)
data Tipo = Piao | Dama deriving (Eq, Show)
data Peca = Peca Jogador Tipo deriving (Eq, Show)
data Casa = Clara | Escura (Maybe Peca) deriving (Eq, Show)

type Tabuleiro = [[Casa]]
type Pos = (Int, Int)  -- Linha, Coluna