module Inicial where

import Control.Concurrent (threadDelay)
import qualified Terminal
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)
import Utils (centralizar, limparTela, terminalWidth,carregarLogo)


mostrarLogoAnimado :: FilePath -> IO ()
mostrarLogoAnimado caminho = do
  linhas <- carregarLogo caminho
  mostrarLinhas linhas
  where
    mostrarLinhas [] = return ()
    mostrarLinhas (l:ls) = do
      putStrLn $ centralizar terminalWidth l
      threadDelay 100000 
      mostrarLinhas ls


paginaInicial :: IO ()
paginaInicial = do
  mostrarLogoAnimado "../banners/plplay.txt"
  threadDelay 1000000
  _ <- getLine
  limparTela
  return ()
