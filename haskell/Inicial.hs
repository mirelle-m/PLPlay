module Inicial where

import Control.Concurrent (threadDelay)
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)
import Terminal qualified
import Utils (carregarLogo, centralizar, limparTela, larguraTerminal)

mostrarLogoAnimada :: FilePath -> IO ()
mostrarLogoAnimada caminho = do
  linhas <- carregarLogo caminho
  mostrarLinhas linhas
  where
    mostrarLinhas [] = return ()
    mostrarLinhas (l : ls) = do
      putStrLn $ centralizar larguraTerminal l
      threadDelay 100000
      mostrarLinhas ls


paginaInicial :: IO ()
paginaInicial = do
  mostrarLogoAnimada "../banners/plplay.txt"
  threadDelay 1000000
  _ <- getLine
  limparTela
  return ()
