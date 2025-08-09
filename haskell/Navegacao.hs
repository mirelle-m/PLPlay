module Navegacao (escolherOpcaoComTitulo) where

import System.IO
import Data.Char (ord)
import Control.Monad (when)

import Utils ( centralizar, terminalWidth, limparTela,limparTelaCompleta,mostrarLogoCentralizado)

configsTemporariasTerminal :: IO a -> IO a
configsTemporariasTerminal action = do
    oldBuffering <- hGetBuffering stdin
    oldEcho <- hGetEcho stdin
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    result <- action
    hSetBuffering stdin oldBuffering
    hSetEcho stdin oldEcho
    return result

escolherOpcaoComTitulo :: FilePath -> [String] -> IO Int
escolherOpcaoComTitulo path opcoes = configsTemporariasTerminal $ go 0
  where
    n = length opcoes
    largura = terminalWidth

    go selectedIndex = do
        -- Limpa a tela inteira
        limparTelaCompleta

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
        
    

data Key = ArrowUp | ArrowDown | Enter | Other deriving (Show, Eq)


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