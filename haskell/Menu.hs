module Menu where

import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Char (isAlphaNum, isDigit, isLower, isUpper)
import Data.List (isInfixOf)
import Data.Typeable (Typeable)
import Utils (centralizar, limparTela, limparTelaCompleta, larguraTerminal, mostrarLogoCentralizada, carregarLogo)
import Inicial (paginaInicial)
import MapaMissoes (escolherMissao, imprimirMapa)
import Navegacao (escolherOpcaoComTitulo)
import System.Directory (doesFileExist)
import Flashcard (iniciarTreino)

menuPrincipal :: IO ()
menuPrincipal = do
    let largura = larguraTerminal
    let opcoes = [ "🎮 Iniciar Novo Jogo"
                 , "📰 Ver Regras do Jogo"
                 , "🗺️  Ver Mapa de Missões"
                 , "🎯 Modo Treino"
                 , "💾 Continuar Jogo"
                 , "🚪 Sair"
                 ]
                          
    escolha <- escolherOpcaoComTitulo "../banners/menu_principal.txt" opcoes
    limparTelaCompleta
    case escolha of
        0 -> do
            putStrLn "Iniciando novo jogo..."
            missao <- escolherMissao
            putStrLn $ "\n🚀 Você selecionou: " ++ missao
            menuPrincipal
        1 -> do
            limparTelaCompleta
            mostrarRegrasJogo

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
        4 -> do
            putStrLn "Continuando jogo"
            missao <- escolherMissao
            putStrLn $ "\n🚀 Você selecionou: " ++ missao
            menuPrincipal
        5 -> do
            putStrLn "Saindo do jogo... Até a próxima! 👋"
        _ -> putStrLn "Opção inválida."


mostrarRegrasJogo:: IO()
mostrarRegrasJogo = do
  mostrarLogoCentralizada "../banners/regras.txt" 
  return ()
