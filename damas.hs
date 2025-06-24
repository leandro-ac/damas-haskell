-- Jogo de Damas em Haskell
-- Autores:
-- Leandro ALvares de Carvalho 202065211A
-- Vinicius ....

import Data.Char (toUpper, chr, ord)
import Data.Maybe (isJust)
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)

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

ehAdversario :: Jogador -> Maybe Peca -> Bool
ehAdversario j (Just (Peca j' _)) = j /= j'
ehAdversario _ _ = False

-- Movimentos possíveis para peões e damas
direcoesPiao :: Jogador -> [(Int, Int)]
direcoesPiao A = [(1, -1), (1, 1)]
direcoesPiao B = [(-1, -1), (-1, 1)]

todosSaltos :: [(Int, Int)]
todosSaltos = [(-1,-1), (-1,1), (1,-1), (1,1)]

movimentosPeca :: Tabuleiro -> Jogador -> Pos -> [Pos]
movimentosPeca tab j (l,c) =
  case emJogo tab (l,c) of
    Just (Peca _ Piao) ->
      [ (l+dl, c+dc) |
        (dl, dc) <- direcoesPiao j,
        dentro (l+dl, c+dc),
        emJogo tab (l+dl, c+dc) == Nothing ]
    Just (Peca _ Dama) ->
      [ (l+i*dl, c+i*dc) |
        (dl,dc) <- todosSaltos, i <- [1..7],
        let pos = (l+i*dl, c+i*dc),
        dentro pos,
        emJogo tab pos == Nothing ]
    _ -> []

-- Captura
capturasPossiveis :: Tabuleiro -> Jogador -> Pos -> [(Pos, Pos)]
capturasPossiveis tab j (l,c) =
  case emJogo tab (l,c) of
    Just (Peca _ Piao) -> 
      [ ((l+2*dl, c+2*dc), (l+dl, c+dc)) |
        (dl,dc) <- todosSaltos,
        let alvo = (l+dl, c+dc),
        let dest = (l+2*dl, c+2*dc),
        dentro alvo, dentro dest,
        ehAdversario j (emJogo tab alvo),
        emJogo tab dest == Nothing ]
    Just (Peca _ Dama) ->
      concatMap (buscaDama (l,c)) todosSaltos
    _ -> []

  where
    buscaDama (r,c) (dl,dc) = 
      let linha = [(r+i*dl, c+i*dc) | i <- [1..7], dentro (r+i*dl, c+i*dc)]
          salto (p1:p2:ps) =
            case (emJogo tab p1, emJogo tab p2) of
              (Just adv, Nothing) | ehAdversario j (Just adv) -> [(p2, p1)]
              (Nothing, _) -> salto (p2:ps)
              _ -> []
          salto _ = []
      in salto linha

-- Executa jogada
mover :: Tabuleiro -> Pos -> Pos -> Tabuleiro
mover tab de para =
  let Just (Peca j t) = emJogo tab de
      t' = if (j == A && fst para == 7) || (j == B && fst para == 0) then Dama else t
      limpa = setar tab de (Escura Nothing)
      capt = if abs (fst para - fst de) == 2
             then let meio = ((fst de + fst para) `div` 2, (snd de + snd para) `div` 2)
                  in setar limpa meio (Escura Nothing)
             else limpa
  in setar capt para (Escura (Just (Peca j t')))

setar :: Tabuleiro -> Pos -> Casa -> Tabuleiro
setar tab (l,c) val =
  take l tab ++ [take c (tab !! l) ++ [val] ++ drop (c+1) (tab !! l)] ++ drop (l+1) tab

-- Entrada
lerPos :: String -> Maybe Pos
lerPos [col, lin]
  | toUpper col `elem` ['A'..'H'], lin `elem` ['1'..'8'] =
      Just (8 - (ord lin - ord '0'), ord (toUpper col) - ord 'A')
lerPos _ = Nothing

-- IA simples: escolhe primeiro movimento válido
jogadaIA :: Tabuleiro -> Jogador -> Maybe (Pos, Pos)
jogadaIA tab j =
  let poss = [ ((l,c), dest) |
                l <- [0..7], c <- [0..7],
                emJogo tab (l,c) == Just (Peca j Piao) || emJogo tab (l,c) == Just (Peca j Dama),
                let caps = capturasPossiveis tab j (l,c),
                (dest, _) <- caps ] ++
             [ ((l,c), dest) |
                l <- [0..7], c <- [0..7],
                emJogo tab (l,c) == Just (Peca j Piao) || emJogo tab (l,c) == Just (Peca j Dama),
                dest <- movimentosPeca tab j (l,c) ]
  in if null poss then Nothing else Just (head poss)

-- Loop do jogo
jogar :: Tabuleiro -> Jogador -> (Bool, Bool) -> IO ()
jogar tab j (ehHumanoA, ehHumanoB) = do
  putStrLn $ "\nTurno do jogador: " ++ show j
  putStrLn $ mostrarTabuleiro tab
  let humano = if j == A then ehHumanoA else ehHumanoB
  if humano then do
    putStr "Digite movimento (ex: A3 B4): "
    hFlush stdout
    entrada <- getLine
    case words entrada of
      [origem, destino] ->
        case (lerPos origem, lerPos destino) of
          (Just de, Just para) ->
            if para `elem` map fst (capturasPossiveis tab j de) || para `elem` movimentosPeca tab j de
              then jogar (mover tab de para) (proximo j) (ehHumanoA, ehHumanoB)
              else putStrLn "Movimento inválido." >> jogar tab j (ehHumanoA, ehHumanoB)
          _ -> erro
      _ -> erro
  else do
    putStrLn "IA está pensando..."
    threadDelay 500000
    case jogadaIA tab j of
      Just (de, para) -> do
        putStrLn $ "IA jogou de " ++ showPos de ++ " para " ++ showPos para
        jogar (mover tab de para) (proximo j) (ehHumanoA, ehHumanoB)
      Nothing -> putStrLn $ "Jogador " ++ show j ++ " não possui movimentos. Fim de jogo."
  where
    proximo A = B
    proximo B = A
    erro = putStrLn "Entrada inválida." >> jogar tab j (ehHumanoA, ehHumanoB)
    showPos (l,c) = [chr (c + 65)] ++ show (8 - l)

-- Menu
menu :: IO ()
menu = do
  putStrLn "\n== JOGO DE DAMAS =="
  putStrLn "1 - Jogar contra IA"
  putStrLn "2 - Ver IA vs IA"
  putStrLn "3 - Sair"
  putStr "Escolha: "
  hFlush stdout
  op <- getLine
  case op of
    "1" -> do
      putStr "Quer ser o jogador A ou B? "
      hFlush stdout
      j <- getLine
      case map toUpper j of
        "A" -> jogar tabuleiroInicial A (True, False)
        "B" -> jogar tabuleiroInicial A (False, True)
        _   -> putStrLn "Escolha inválida." >> menu
    "2" -> jogar tabuleiroInicial A (False, False)
    "3" -> putStrLn "Saindo..."
    _   -> putStrLn "Opção inválida." >> menu

main :: IO ()
main = do
  putStrLn "Bem-vindo ao jogo de Damas!"
  menu