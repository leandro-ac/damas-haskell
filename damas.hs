-- Jogo de Damas em Haskell
-- Leandro Alvares de Carvalho 202065211A
-- Vinicius

import Data.Char (toUpper, chr, ord)
import Data.Maybe (isJust)
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)

-- Tipos principais
data Jogador = A | B deriving (Eq, Show)
data Tipo = Piao | Dama deriving (Eq, Show)
data Peca = Peca Jogador Tipo deriving (Eq, Show)
data Casa = Clara | Escura (Maybe Peca) deriving (Eq, Show)

type Tabuleiro = [[Casa]]
type Pos = (Int, Int)

-- Inicializa o tabuleiro com peças nas posições iniciais
tabuleiroInicial :: Tabuleiro
tabuleiroInicial = [ [inicial r c | c <- [0..7]] | r <- [0..7] ]
  where
    -- Cada casa par é escura e pode ter peça, as outras são claras (vazias)
    inicial l c
      | (l + c) `mod` 2 == 0 = Escura (pecaInicial l)
      | otherwise            = Clara
    -- Define as peças iniciais para os três primeiros e três últimos filas
    pecaInicial l
      | l <= 2 = Just (Peca A Piao)
      | l >= 5 = Just (Peca B Piao)
      | otherwise = Nothing

-- Exibição de uma casa no tabuleiro
mostrarCasa :: Casa -> String
mostrarCasa Clara = " . "                   -- Casa clara representada por ponto
mostrarCasa (Escura Nothing) = "   "         -- Casa escura vazia é espaço em branco
mostrarCasa (Escura (Just (Peca A Piao))) = " a "  -- Peão jogador A minúsculo
mostrarCasa (Escura (Just (Peca A Dama))) = " A "  -- Dama jogador A maiúsculo
mostrarCasa (Escura (Just (Peca B Piao))) = " b "  -- Peão jogador B minúsculo
mostrarCasa (Escura (Just (Peca B Dama))) = " B "  -- Dama jogador B maiúsculo

-- Exibe o tabuleiro completo em formato legível, com colunas e linhas rotuladas
mostrarTabuleiro :: Tabuleiro -> String
mostrarTabuleiro tab =
  let cabecalho = "   " ++ concat [ " " ++ [chr (c + 65)] ++ " " | c <- [0..7] ]  -- Letras A-H no topo
      linhas = [ show (r + 1) ++ " " ++ concatMap mostrarCasa linha | (r, linha) <- zip [0..] tab ]
  in unlines (cabecalho : linhas)

-- Retorna a peça na posição dada se estiver dentro do tabuleiro e for casa escura
emJogo :: Tabuleiro -> Pos -> Maybe Peca
emJogo tab (l,c)
  | l < 0 || l > 7 || c < 0 || c > 7 = Nothing
  | otherwise = case tab !! l !! c of
                  Escura mp -> mp
                  _ -> Nothing

-- Verifica se a posição está dentro do tabuleiro
dentro :: Pos -> Bool
dentro (l,c) = l >= 0 && l <= 7 && c >= 0 && c <= 7

-- Verifica se uma peça pertence ao jogador adversário
ehAdversario :: Jogador -> Maybe Peca -> Bool
ehAdversario j (Just (Peca j' _)) = j /= j'
ehAdversario _ _ = False

-- Direções válidas de movimento para peões (A anda para baixo, B para cima)
direcoesPiao :: Jogador -> [(Int, Int)]
direcoesPiao A = [(1, -1), (1, 1)]
direcoesPiao B = [(-1, -1), (-1, 1)]

-- Todas as direções possíveis de salto (diagonais)
todosSaltos :: [(Int, Int)]
todosSaltos = [(-1,-1), (-1,1), (1,-1), (1,1)]

-- Lista movimentos possíveis para uma peça na posição dada
movimentosPeca :: Tabuleiro -> Jogador -> Pos -> [Pos]
movimentosPeca tab j (l,c) =
  case emJogo tab (l,c) of
    Just (Peca _ Piao) ->
      [ (l+dl, c+dc) |                    -- Movimento simples do peão
        (dl, dc) <- direcoesPiao j,
        dentro (l+dl, c+dc),
        emJogo tab (l+dl, c+dc) == Nothing ]
    Just (Peca _ Dama) ->
      [ (l+i*dl, c+i*dc) |                -- Movimento livre da dama nas diagonais
        (dl,dc) <- todosSaltos, i <- [1..7],
        let pos = (l+i*dl, c+i*dc),
        dentro pos,
        emJogo tab pos == Nothing ]
    _ -> []

-- Calcula as capturas possíveis para peões e damas na posição dada
capturasPossiveis :: Tabuleiro -> Jogador -> Pos -> [(Pos, Pos)]
capturasPossiveis tab j (l,c) =
  case emJogo tab (l,c) of
    Just (Peca _ Piao) -> 
      [ ((l+2*dl, c+2*dc), (l+dl, c+dc)) |   -- Destino e posição da peça capturada
        (dl,dc) <- todosSaltos,
        let alvo = (l+dl, c+dc),
        let dest = (l+2*dl, c+2*dc),
        dentro alvo, dentro dest,
        ehAdversario j (emJogo tab alvo),
        emJogo tab dest == Nothing ]
    Just (Peca _ Dama) ->
      concatMap (buscaDama (l,c)) todosSaltos   -- Capturas possíveis para dama em todas direções
    _ -> []

  where
    -- Para damas, percorre na direção indicada procurando adversário seguido de casa vazia para captura
    buscaDama (r,c) (dl,dc) = 
      let linha = [(r+i*dl, c+i*dc) | i <- [1..7], dentro (r+i*dl, c+i*dc)]
          salto (p1:p2:ps) =
            case (emJogo tab p1, emJogo tab p2) of
              (Just adv, Nothing) | ehAdversario j (Just adv) -> [(p2, p1)]  -- Captura válida encontrada
              (Nothing, _) -> salto (p2:ps)    -- Continua procurando na linha
              _ -> []
          salto _ = []
      in salto linha

-- Realiza o movimento da peça no tabuleiro
-- Se for um movimento de captura, remove a peça capturada
-- Se o peão chegar à última linha adversária, promove a dama
mover :: Tabuleiro -> Pos -> Pos -> Tabuleiro
mover tab de para =
  let Just (Peca j t) = emJogo tab de
      -- Promove peão a dama ao alcançar última linha
      t' = if (j == A && fst para == 7) || (j == B && fst para == 0) then Dama else t
      limpa = setar tab de (Escura Nothing)   -- Remove peça da posição original
      capt = if abs (fst para - fst de) == 2  -- Movimento de salto (captura)
             then let meio = ((fst de + fst para) `div` 2, (snd de + snd para) `div` 2)
                  in setar limpa meio (Escura Nothing)  -- Remove peça capturada
             else limpa
  in setar capt para (Escura (Just (Peca j t'))) -- Coloca peça na posição destino

-- Atualiza o tabuleiro na posição especificada com a nova casa
setar :: Tabuleiro -> Pos -> Casa -> Tabuleiro
setar tab (l,c) val =
  take l tab ++ [take c (tab !! l) ++ [val] ++ drop (c+1) (tab !! l)] ++ drop (l+1) tab

-- Lê uma posição no formato "A3" e converte para coordenadas internas (linha, coluna)
lerPos :: String -> Maybe Pos
lerPos [col, lin]
  | toUpper col `elem` ['A'..'H'], lin `elem` ['1'..'8'] =
      Just ((ord lin - ord '1'), ord (toUpper col) - ord 'A')
lerPos _ = Nothing

-- IA simples: escolhe o primeiro movimento válido encontrado (captura ou movimento)
jogadaIA :: Tabuleiro -> Jogador -> Maybe (Pos, Pos)
jogadaIA tab j =
  let poss = [ ((l,c), dest) |                -- Capturas possíveis primeiro
                l <- [0..7], c <- [0..7],
                emJogo tab (l,c) == Just (Peca j Piao) || emJogo tab (l,c) == Just (Peca j Dama),
                let caps = capturasPossiveis tab j (l,c),
                (dest, _) <- caps ] ++
             [ ((l,c), dest) |                 -- Depois movimentos simples
                l <- [0..7], c <- [0..7],
                emJogo tab (l,c) == Just (Peca j Piao) || emJogo tab (l,c) == Just (Peca j Dama),
                dest <- movimentosPeca tab j (l,c) ]
  in if null poss then Nothing else Just (head poss)

-- Loop principal do jogo
-- Exibe tabuleiro, lê jogada do jogador humano ou IA, verifica validade e alterna vezes
jogar :: Tabuleiro -> Jogador -> (Bool, Bool) -> IO ()
jogar tab j (ehHumanoA, ehHumanoB) = do
  putStrLn $ "\nVez do jogador: " ++ show j
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
            -- Verifica se destino é válido (movimento normal ou captura)
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
    -- Exibe posição no formato padrão (ex: A3)
    showPos (l,c) = [chr (c + 65)] ++ show (l + 1)

-- Menu principal do jogo
main :: IO ()
main = do
  loop
  where
    loop = do
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
            "B" -> jogar tabuleiroInicial B (False, True)
            _   -> putStrLn "Escolha inválida." >> loop
        "2" -> jogar tabuleiroInicial A (False, False)
        "3" -> putStrLn "Saindo..."
        _   -> putStrLn "Opção inválida." >> loop