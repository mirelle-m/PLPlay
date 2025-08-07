module Auth where

import System.IO
import System.Directory (doesFileExist)
import Control.Monad (when)
import Data.List (isInfixOf)
import qualified Terminal
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)
import Control.Concurrent (threadDelay)

terminalSize = unsafeDupablePerformIO Terminal.getTermSize
terminalHeight = fst terminalSize 
terminalWidth = snd terminalSize 

mainAuth :: IO ()
mainAuth = do
    let largura = terminalWidth
    putStrLn $ replicate largura '='
    putStrLn $ centralizar largura "🔐 LOGIN/CADASTRO"
    putStrLn $ replicate largura '='
    putStrLn "Digite seu username:"
    username <- getLine
    putStrLn "Digite sua senha:"
    senha <- getLine
    let userData = "username: " ++ username ++ "\nprogresso: Missao3 csharp\n"
    limparTela
    
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
                    mostrarRegrasJogo
                    appendFile "user.txt" userData
                    putStrLn "✅ Cadastro realizado com sucesso!"
                    showMenu
        else do
            appendFile "user.txt" userData
            putStrLn "Primeiro usuário cadastrado com sucesso!"
            showMenu


mostrarRegrasJogo:: IO()
mostrarRegrasJogo = do
  let largura = terminalWidth
  putStrLn $ replicate largura '='
  putStrLn $ centralizar largura "📘 Regras do PLPlay "
  threadDelay 100000
  putStrLn "🎯 Missões são quizzes sobre temas de cada estágio da disciplina PLP."
  threadDelay 10000
  putStrLn "🔒 Missões são desbloqueadas uma por vez — conclua uma para liberar a próxima!"
  threadDelay 10000
  putStrLn "❗ Limite de erros por missão:"
  threadDelay 10000
  putStrLn "    🟢 Fácil: até 3 erros"
  threadDelay 10000
  putStrLn "    🟡 Médio: até 2 erros"
  threadDelay 10000
  putStrLn "    🔴 Difícil: 1 erro"
  threadDelay 10000
  putStrLn "💥 Se ultrapassar o limite, a missão reinicia do zero"
  threadDelay 10000
  putStrLn "🏆 Vença chefões e conquiste medalhas com seu desempenho"
  threadDelay 10000
  putStrLn "📚 Revise perguntas erradas no modo Treino (flashcards)"
  threadDelay 10000
  putStrLn "📈 Aprenda jogando e avance até o final da jornada!"
  threadDelay 10000
  putStrLn $ replicate largura '='
  limparTela

showMenu :: IO ()
showMenu = do
    let largura = terminalWidth
    putStrLn $ replicate largura '='
    putStrLn $ centralizar largura "MENU PRINCIPAL"
    putStrLn $ replicate largura '='
    putStrLn "[1] 🎮 Iniciar Novo Jogo"
    putStrLn "[2] 💾 Continuar Jogo"
    putStrLn "[3] 🚪 Sair"
    putStrLn $ replicate largura '='
    putStr "Escolha uma opção: "

centralizar :: Int -> String -> String
centralizar largura texto =
    let espacos = replicate ((largura - length texto) `div` 2) ' '
    in espacos ++ texto

limparTela :: IO ()
limparTela = putStr "\ESC[2J\ESC[H"