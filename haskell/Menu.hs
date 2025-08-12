module Menu where

import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Char (isAlphaNum, isDigit, isLower, isUpper)
import Data.List (isInfixOf)
import Data.Typeable (Typeable)
import Utils 
import Inicial 
import Navegacao 
import Missoes 
import Jogo 
import System.Directory (doesFileExist)
import Flashcard (iniciarTreino)

menuPrincipal :: IO ()
menuPrincipal = do
    let largura = larguraTerminal
    let opcoes = [ "🎮 Jogar"
                 , "📰 Ver Regras do Jogo"
                 , "🗺️  Ver Mapa de Missões"
                 , "🎯 Modo Treino"
                 , "🚪 Sair"
                 ]
                          
    escolha <- escolherOpcaoComTitulo "../banners/menu_principal.txt" opcoes
    limparTelaCompleta
    case escolha of
        0 -> do
            putStrLn "Iniciando novo jogo..."
            menuJogo
            menuPrincipal
        1 -> do
            limparTelaCompleta
            mostrarLogoCentralizada "../banners/regras.txt"
            putStrLn "\nPressione Enter para voltar ao menu..."
            _ <- getLine
            menuPrincipal
        2 -> do
            limparTelaCompleta
            imprimirMapa
            putStrLn "\nPressione Enter para voltar ao menu..."
            _ <- getLine
            menuPrincipal
        3 -> do
            putStrLn "Modo Treino"
            iniciarTreino []
            menuPrincipal
            menuPrincipal
        4 -> do
            putStrLn "Saindo do jogo... Até a próxima! 👋"
        _ -> putStrLn "Opção inválida."

