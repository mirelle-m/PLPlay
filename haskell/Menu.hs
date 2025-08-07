module Menu where
import System.Directory (doesFileExist)
import Control.Concurrent (threadDelay)
import Data.List (isInfixOf)

-- imports de outros modulos
import Utils (centralizar, limparTela, terminalWidth)
import Navegacao (escolherOpcao)
import MapaMissoes (imprimirMapa, escolherMissao)
import Inicial (paginaInicial)


executarAplicacao :: IO ()
executarAplicacao = do
    paginaInicial
    authSuccess <- autenticar
    if authSuccess
        then do
            menuPrincipal
        else
            putStrLn "❌ Falha na autenticação."


menuPrincipal :: IO ()
menuPrincipal = do
    let largura = terminalWidth
    let opcoes = [ "🎮 Iniciar Novo Jogo"
                 , "📰 Ver Regras do Jogo"
                 , "🗺️  Ver Mapa de Missões"
                 , "💾 Continuar Jogo"
                 , "🚪 Sair"
                 ]

    putStrLn $ replicate largura '='
    putStrLn $ centralizar largura "MENU PRINCIPAL"
    putStrLn $ replicate largura '='
    escolha <- escolherOpcao opcoes

    case escolha of
        0 -> do
            putStrLn "Iniciando novo jogo..."
            missao <- escolherMissao
            -- colocar funcao para realizar o que foi desejado
            putStrLn $ "\n🚀 Você selecionou: " ++ missao
            menuPrincipal
        1 -> do
            putStrLn "Mostrando Regras do jogo..."
            -- colocar funcao para realizar o que foi desejado
            mostrarRegrasJogo
            menuPrincipal
        2 -> do
            putStrLn "Mostrando Mapa de Missões"
            imprimirMapa
            menuPrincipal
        3 -> do
            putStrLn "Continuando jogo"
            missao <- escolherMissao
            putStrLn $ "\n🚀 Você selecionou: " ++ missao
            menuPrincipal
        4 -> do
            putStrLn "Saindo do jogo... Até a próxima! 👋"
        _ -> putStrLn "Opção inválida."

-- Retorna True se autenticado ou cadastrado com sucesso
autenticar :: IO Bool
autenticar = do
    let largura = terminalWidth
    putStrLn $ replicate largura '='
    putStrLn $ centralizar largura "🔐 AUTENTICAÇÃO"
    putStrLn $ replicate largura '='
    putStrLn "Digite seu username:"
    username <- getLine
    putStrLn "Digite sua senha:"
    _ <- getLine  -- senha não é usada, mas mantida para o fluxo
    let userData = "username: " ++ username ++ "\nprogresso: Missao3 csharp\n"

    exists <- doesFileExist "user.txt"
    if exists
        then do
            content <- readFile "user.txt"
            if ("username: " ++ username) `isInfixOf` content
                then do
                    putStrLn "✅ Autenticado com sucesso!"
                    return True
                else do
                    putStrLn "Usuário não encontrado. Cadastrando novo usuário..."
                    appendFile "user.txt" userData
                    putStrLn "✅ Cadastro realizado com sucesso!"
                    return True
        else do
            writeFile "user.txt" userData
            putStrLn "Primeiro usuário cadastrado com sucesso!"
            return True



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