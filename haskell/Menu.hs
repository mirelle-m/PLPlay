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
      putStrLn "Iniciando Novo Jogo..."
      missao <- escolherMissao
      putStrLn $ "\n Você selecionou: " ++ missao
      menuPrincipal
    1 -> do
      putStrLn "Mostrando Regras do Jogo..."
      mostrarRegrasJogo "../banners/regras.txt"
      putStrLn "\nPressione Enter para voltar ao menu..."
      _ <- getLine
      menuPrincipal
    2 -> do
      putStrLn "Mostrando Mapa de Missões"
      mostrarRegrasJogo "../banners/mapa.txt"
      putStrLn "\nPressione Enter para voltar ao menu..."
      _ <- getLine
      menuPrincipal
    3 -> do
      putStrLn "Modo Treino"
      menuPrincipal
    4 -> do
      putStrLn "Continuando jogo"
      missao <- escolherMissao
      putStrLn $ "\nVocê selecionou: " ++ missao
      menuPrincipal
    5 -> do
      putStrLn "Saindo do jogo... Até a próxima!"
    _ -> putStrLn "Opção inválida." >> menuPrincipal


mostrarRegrasJogo :: FilePath -> IO ()
mostrarRegrasJogo caminho = do
  linhas <- carregarLogo caminho
  mostrarLinhas linhas
  where
    mostrarLinhas [] = return ()
    mostrarLinhas (l : ls) = do
      putStrLn $ centralizar larguraTerminal l
      threadDelay 100000
      mostrarLinhas ls
