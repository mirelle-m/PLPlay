module Menu where
import System.Directory (doesFileExist)
import Control.Concurrent (threadDelay)
import Data.List (isInfixOf)

-- imports de outros modulos
import Utils (centralizar, limparTela, terminalWidth, mostrarLogoCentralizado,limparTelaCompleta)
import Navegacao (escolherOpcaoComTitulo)
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
            -- colocar funcao para realizar o que foi desejado
            putStrLn $ "\n🚀 Você selecionou: " ++ missao
            menuPrincipal
        1 -> do
            limparTelaCompleta
            -- colocar funcao para realizar o que foi desejado
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
            putStrLn "Continuando jogo"
            missao <- escolherMissao
            putStrLn $ "\n🚀 Você selecionou: " ++ missao
            menuPrincipal
        4 -> do
            putStrLn "Saindo do jogo... Até a próxima! 👋"
        _ -> putStrLn "Opção inválida."

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
  mostrarLogoCentralizado "../banners/regras.txt" 
  return ()