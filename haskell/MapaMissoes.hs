module MapaMissoes where

import qualified Terminal
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.List (isPrefixOf)

terminalSize = unsafeDupablePerformIO Terminal.getTermSize
terminalHeight = fst terminalSize
terminalWidth = snd terminalSize

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

centralizar :: Int -> String -> String
centralizar largura texto =
  let espacos = replicate ((largura - length texto) `div` 2) ' '
  in espacos ++ texto
