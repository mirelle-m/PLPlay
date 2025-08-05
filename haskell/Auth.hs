module Auth where

import System.IO
import System.Directory (doesFileExist)
import Control.Monad (when)
import Data.List (isInfixOf)
import qualified Terminal
import System.IO.Unsafe (unsafeDupablePerformIO)
import Navegacao (escolherOpcao)

terminalSize = unsafeDupablePerformIO Terminal.getTermSize
terminalHeight = fst terminalSize 
terminalWidth = snd terminalSize 

mainAuth :: IO ()
mainAuth = do
    let largura = terminalWidth
    putStrLn $ replicate largura '='
    putStrLn $ centralizar largura "🔐 AUTENTICAÇÃO"
    putStrLn $ replicate largura '='
    putStrLn "Digite seu username:"
    username <- getLine
    putStrLn "Digite sua senha:"
    senha <- getLine
    let userData = "username: " ++ username ++ "\nprogresso: Missao3 csharp\n"

    exists <- doesFileExist "user.txt"
    if exists
        then do
            content <- readFile "user.txt"
            if ("username: " ++ username) `isInfixOf` content
                then do
                    putStrLn "✅ Autenticado com sucesso!"
                    showMenu
                else do
                    putStrLn "Usuário não encontrado. Cadastrando novo usuário..."
                    writeFile "user.txt" userData
                    putStrLn "✅ Cadastro realizado com sucesso!"
                    showMenu
        else do
            writeFile "user.txt" userData
            putStrLn "Primeiro usuário cadastrado com sucesso!"
            showMenu

showMenu :: IO ()
showMenu = do
    let largura = terminalWidth
    let opcoes = [ "🎮 Iniciar Novo Jogo"
                 , "💾 Continuar Jogo"
                 , "🚪 Sair"
                 ]
    
    putStrLn $ replicate largura '='
    putStrLn $ centralizar largura "MENU PRINCIPAL"
    putStrLn $ replicate largura '='

    escolha <- escolherOpcao opcoes

    case escolha of
        0 -> putStrLn "Iniciando novo jogo..."
        1 -> putStrLn "Carregando jogo salvo..."
        2 -> putStrLn "Saindo do jogo..."
        _ -> putStrLn "Opção inválida."

centralizar :: Int -> String -> String
centralizar largura texto =
    let espacos = replicate ((largura - length texto) `div` 2) ' '
    in espacos ++ texto
