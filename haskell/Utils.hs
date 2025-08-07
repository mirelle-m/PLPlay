module Utils (centralizar,limparTela, terminalWidth, terminalHeight,carregarLogo ) where

import qualified Terminal
import System.IO.Unsafe (unsafeDupablePerformIO)

terminalSize = unsafeDupablePerformIO Terminal.getTermSize
terminalHeight = fst terminalSize
terminalWidth = snd terminalSize

centralizar :: Int -> String -> String
centralizar largura texto =
    let espacos = replicate ((largura - length texto) `div` 2) ' '
    in espacos ++ texto


limparTela :: IO ()
limparTela = putStr "\ESC[2J\ESC[H"

carregarLogo :: FilePath -> IO [String]
carregarLogo caminho = do
  conteudo <- readFile caminho
  return (lines conteudo)
