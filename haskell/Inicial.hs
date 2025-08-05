module Inicial where

import Control.Concurrent (threadDelay)
import qualified Terminal
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)

terminalSize = unsafeDupablePerformIO Terminal.getTermSize
terminalHeight = fst terminalSize 
terminalWidth = snd terminalSize 

logo :: [String]
logo =
  [ " _____  _      _____  _           __     __"
  , "|  __ \\| |    |  __ \\| |        /\\ \\   / /"
  , "| |__) | |    | |__) | |       /  \\ \\_/ / "
  , "|  ___/| |    |  ___/| |      / /\\ \\   /  "
  , "| |    | |____| |    | |____ / ____ \\| |  "
  , "|_|    |______|_|    |______/_/    \\_\\_|  "
  , "                                          "
  , "         Bem-vindo(a) ao PLPlay 📚             "
  ]

centralizar :: Int -> String -> String
centralizar largura texto =
  let espacos = replicate ((largura - length texto) `div` 2) ' '
  in espacos ++ texto

mostrarLogoAnimado :: IO ()
mostrarLogoAnimado = mostrarLinhas logo
  where
    mostrarLinhas [] = return ()
    mostrarLinhas (l:ls) = do
      putStrLn $ centralizar terminalWidth l
      threadDelay 300000  -- 0.3 segundo
      mostrarLinhas ls

paginaInicial :: IO ()
paginaInicial = do
  putStrLn $ replicate terminalWidth '='
  mostrarLogoAnimado
  putStrLn $ replicate terminalWidth '='
  threadDelay 1000000
  putStrLn "\nPressione ENTER para continuar..."
  _ <- getLine
  return ()
