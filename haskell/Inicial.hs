module Inicial where

import Control.Concurrent (threadDelay)
import qualified Terminal
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)

terminalSize = unsafeDupablePerformIO Terminal.getTermSize
terminalHeight = fst terminalSize 
terminalWidth = snd terminalSize 

-- Arte ASCII do nome do jogo
logo :: [String]
logo =
  [ " _____  _      _____  _           __     __"
  , "|  __ \\| |    |  __ \\| |        /\\ \\   / /"
  , "| |__) | |    | |__) | |       /  \\ \\_/ / "
  , "|  ___/| |    |  ___/| |      / /\\ \\   /  "
  , "| |    | |____| |    | |____ / ____ \\| |  "
  , "|_|    |______|_|    |______/_/    \\_\\_|  "
  , "                                          "
  , "         Bem-vindo(a) ao PLPlay              "
  ]


-- Mostra a arte, linha por linha, com atraso
mostrarLogoAnimado :: IO ()
mostrarLogoAnimado = mostrarLinhas logo
  where
    mostrarLinhas [] = return ()
    mostrarLinhas (l:ls) = do
      putStrLn l
      threadDelay 300000  -- 0.3 segundos
      mostrarLinhas ls

-- Tela inicial
paginaInicial :: IO ()
paginaInicial = do
  putStrLn "=============================================="
  mostrarLogoAnimado
  putStrLn "=============================================="
  threadDelay 1000000  -- espera 1 segundo
  putStrLn "\nPressione ENTER para continuar..."
  _ <- getLine
  return ()