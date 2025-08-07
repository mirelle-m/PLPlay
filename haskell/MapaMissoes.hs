module MapaMissoes where

import qualified Terminal
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.List (isPrefixOf)
import Utils(centralizar,terminalWidth)
import Navegacao (escolherOpcao)

missoesMapeadas :: [(String, [String])]
missoesMapeadas =
  [ ("Primeiro Estágio"
    , [ "Missão 1: Introdução - Históricos e Características"
      , "Missão 2: Classificação e Características"
      , "Missão 3: Valores, Tipos e Sistema de Tipos"
      , "Missão 4: Paradigma Imperativo"
      , "Chefão 1: Batalha dos Fundamentos"
      ])
  , ("Segundo Estágio"
    , [ "Missão 5: Paradigma Funcional"
      , "Chefão 2: Guardião da Recursão"
      ])
  , ("Terceiro Estágio"
    , [ "Missão 6: Paradigma Lógico"
      , "Chefão 3: Mestre da Dedução"
      ])
  ]

gerarLinhasMapa :: [(String, [String])] -> [String]
gerarLinhasMapa = concatMap gerarLinhasEstagio
  where
    gerarLinhasEstagio (estagio, missoes) =
      let titulo = "📍 " ++ estagio
          linhasMissoes = map (\m -> "   " ++ emoji m ++ m) missoes
      in titulo : linhasMissoes

    emoji m = if "Chefão" `elem` words m then "👾 " else "🧭 "

imprimirMapa :: IO ()
imprimirMapa = do
  let largura = terminalWidth
  putStrLn $ replicate largura '='
  putStrLn $ centralizar terminalWidth "MAPA DE MISSÕES"
  putStrLn $ replicate largura '='
  mapM_ putStrLn (gerarLinhasMapa missoesMapeadas)
  putStrLn $ replicate largura '='


escolherMissao :: IO String
escolherMissao = do
  let nomesEstagios = map fst missoesMapeadas
  let estagios = map ("📍 " ++) nomesEstagios
  putStrLn "\n🌍 Escolha um estágio:"
  idxEstagio <- escolherOpcao estagios
  let (nomeEstagio, missoes) = missoesMapeadas !! idxEstagio
  escolherMissaoDoEstagio nomeEstagio missoes


escolherMissaoDoEstagio :: String -> [String] -> IO String
escolherMissaoDoEstagio nomeEstagio missoes = do
  let largura = terminalWidth
  putStrLn $ replicate largura '='
  putStrLn $ centralizar largura $ "🧭 " ++ nomeEstagio
  putStrLn $ replicate largura '='
  idxMissao <- escolherOpcao (map formatarMissao missoes)
  return (missoes !! idxMissao)


formatarMissao :: String -> String
formatarMissao m =
  let emoji = if "Chefão" `elem` words m then "👾 " else "🧭 "
  in emoji ++ m