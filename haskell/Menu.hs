module Menu (showMenu) where

import Auth (autenticar)
import Utils (centralizar, terminalWidth)
import Navegacao (escolherOpcao)

showMenu :: IO ()
showMenu = do
    authSuccess <- autenticar
    if authSuccess
        then do
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
        else
            putStrLn "❌ Falha na autenticação."
