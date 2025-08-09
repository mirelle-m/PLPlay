module Utils (centralizar, limparTela, larguraTerminal, alturaTerminal, carregarLogo) where

import qualified Terminal
import System.IO.Unsafe (unsafeDupablePerformIO)

tamanhoTerminal = unsafeDupablePerformIO Terminal.getTermSize
alturaTerminal = fst tamanhoTerminal
larguraTerminal = snd tamanhoTerminal

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
