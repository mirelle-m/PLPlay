
module Utils where

import qualified Terminal
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.IO

tamanhoTerminal = unsafeDupablePerformIO Terminal.getTermSize
alturaTerminal = fst tamanhoTerminal
larguraTerminal = snd tamanhoTerminal

centralizar :: Int -> String -> String
centralizar largura texto =
    let espacos = replicate ((largura - length texto) `div` 2) ' '
    in espacos ++ texto


limparTela :: IO ()
limparTela = do 
              putStr "\ESC[2J\ESC[H"
              hFlush stdout


limparTelaCompleta :: IO ()
limparTelaCompleta = do
  putStr "\ESC[3J\ESC[2J\ESC[H"
  hFlush stdout


preencherDireita :: Int -> String -> String
preencherDireita n s = s ++ replicate (n - length s) ' '


carregarLogo :: FilePath -> IO [String]
carregarLogo caminho = do
  conteudo <- readFile caminho
  return (lines conteudo)


centralizarBloco :: Int -> [String] -> [String]
centralizarBloco largura linhas =
  let larguraMax = maximum (map length linhas)
      padded = map (preencherDireita larguraMax) linhas
      deslocamento = max 0 ((largura - larguraMax) `div` 2)
      espacos = replicate deslocamento ' '
  in map (espacos ++) padded


mostrarLogoCentralizada :: FilePath -> IO ()
mostrarLogoCentralizada caminho = do
  linhas <- fmap lines (readFile caminho)
  mapM_ putStrLn (centralizarBloco larguraTerminal linhas)


removeAspas :: String -> String
removeAspas str
  | length str >= 2 && head str == '"' && last str == '"' = init (tail str)
  | otherwise = str


adicionaAspas :: String -> String
adicionaAspas str = "\"" ++ str ++ "\""