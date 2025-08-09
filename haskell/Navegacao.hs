module Navegacao (escolherOpcaoComTitulo) where

import System.IO
import Data.Char (ord)
import Control.Monad (when)
import Utils ( centralizar, terminalWidth, limparTela,limparTelaCompleta,mostrarLogoCentralizado)

-- | Desabilita o buffer e o eco do terminal, executa a ação e depois restaura as configurações.
withTerminalSettings :: IO a -> IO a
withTerminalSettings action = do
    oldBuffering <- hGetBuffering stdin
    oldEcho <- hGetEcho stdin
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    result <- action
    hSetBuffering stdin oldBuffering
    hSetEcho stdin oldEcho
    return result



escolherOpcaoComTitulo :: FilePath -> [String] -> IO Int
escolherOpcaoComTitulo path opcoes = withTerminalSettings $ go 0
  where
    n = length opcoes
    largura = terminalWidth

    go selectedIndex = do
        -- Limpa a tela inteira

        mostrarLogoCentralizado path 

        -- Lista de opções
        mapM_ (uncurry exibirOpcao) (zip [0..] opcoes)

        -- Captura tecla
        key <- getKey
        case key of
            "UP"    -> go ((selectedIndex - 1 + n) `mod` n)
            "DOWN"  -> go ((selectedIndex + 1) `mod` n)
            "ENTER" -> return selectedIndex
            _       -> go selectedIndex

    
        where
        exibirOpcao :: Int -> String -> IO ()
        exibirOpcao index texto =
            if index == selectedIndex
                then putStrLn $ "-> " ++ texto
                else putStrLn $ "   " ++ texto
        
        limparTelaCompleta

-- | Enum para representar as teclas especiais que vamos capturar.
data Key = ArrowUp | ArrowDown | Enter | Other deriving (Show, Eq)

-- | Lê uma sequência de teclas e a interpreta como uma `Key`.
-- | Lê uma sequência de teclas e a interpreta como uma `Key`.
-- Captura de tecla especial (setas e Enter)
getKey :: IO String
getKey = do
    c1 <- getChar
    if c1 == '\ESC' then do
        c2 <- getChar
        c3 <- getChar
        case c3 of
            'A' -> return "UP"
            'B' -> return "DOWN"
            _   -> return ""
    else if c1 == '\r' || c1 == '\n' then
        return "ENTER"
    else
        return ""