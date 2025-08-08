module Menu where

import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Char (isAlphaNum, isDigit, isLower, isUpper)
import Data.List (isInfixOf)
import Data.Typeable (Typeable)
import Inicial (paginaInicial)
import MapaMissoes (escolherMissao, imprimirMapa)
import Navegacao (escolherOpcao)
import System.Directory (doesFileExist)
import Utils (carregarLogo, centralizar, limparTela, terminalWidth)

menuPrincipal :: IO ()
menuPrincipal = do
  let largura = terminalWidth
  let opcoes =
        [ "🎮 Iniciar Novo Jogo",
          "📰 Ver Regras do Jogo",
          "🗺️  Ver Mapa de Missões",
          "💾 Continuar Jogo",
          "🚪 Sair"
        ]

  putStrLn $ replicate largura '='
  putStrLn $ centralizar largura "MENU PRINCIPAL"
  putStrLn $ replicate largura '='
  escolha <- escolherOpcao opcoes

  case escolha of
    0 -> do
      putStrLn "Iniciando novo jogo..."
      missao <- escolherMissao
      putStrLn $ "\n Você selecionou: " ++ missao
      menuPrincipal
    1 -> do
      putStrLn "Mostrando Regras do jogo..."
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
      putStrLn "Continuando jogo"
      missao <- escolherMissao
      putStrLn $ "\n Você selecionou: " ++ missao
      menuPrincipal
    4 -> do
      putStrLn "Saindo do jogo... Até a próxima!"
    _ -> putStrLn "Opção inválida." >> menuPrincipal

mostrarRegrasJogo :: FilePath -> IO ()
mostrarRegrasJogo caminho = do
  linhas <- carregarLogo caminho
  mostrarLinhas linhas
  where
    mostrarLinhas [] = return ()
    mostrarLinhas (l : ls) = do
      putStrLn $ centralizar terminalWidth l
      threadDelay 100000
      mostrarLinhas ls
