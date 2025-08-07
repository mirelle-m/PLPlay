module Inicial where

import Control.Concurrent (threadDelay)
import qualified Terminal
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)

terminalSize = unsafeDupablePerformIO Terminal.getTermSize
terminalHeight = fst terminalSize 
terminalWidth = snd terminalSize 

centralizar :: Int -> String -> String
centralizar largura texto =
  let espacos = replicate ((largura - length texto) `div` 2) ' '
  in espacos ++ texto


carregarLogo :: FilePath -> IO [String]
carregarLogo caminho = do
  conteudo <- readFile caminho
  return (lines conteudo)

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
  putStrLn "\nPressione ENTER para continuar..."
  _ <- getLine
  limparTela
  return ()

limparTela :: IO ()
limparTela = putStr "\ESC[2J\ESC[H"
