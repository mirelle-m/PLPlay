module Jogo where

import Missoes
import Usuario
import Data.List (filter, take)
import Data.Maybe (fromMaybe, isNothing)
import System.CPUTime (getCPUTime)
import Text.Read (readMaybe)  
import Navegacao(escolherOpcao,escolherOpcaoComTitulo)
import Utils 
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

menuJogo :: IO ()
menuJogo = do

  let opcoes = [ "💾 Continuar Jogo"
                ,"🎮 Iniciar Novo Jogo"
                 ,"🚪 Voltar"
                 ]

  opcaoMenu <- escolherOpcaoComTitulo "../banners/menu_principal.txt" opcoes
  
  case opcaoMenu of
    0 -> continuarJogo
    1 -> novoJogo
    2 -> return()
    _ -> do
      putStrLn "Opção inválida! Tentando continuar jogo existente..."
      continuarJogo

continuarJogo :: IO ()
continuarJogo = do
  userName <- recuperaLoginAtual
  usuario <- carregaUsuario userName
  
  case usuario of
    Nothing -> do
      putStrLn "Nenhum usuário encontrado. Será criado um novo jogo."
      novoJogo
    Just u -> do
      putStrLn ("Bem vindo de volta, " ++ (nomeUsuario u) ++ "!")
      putStrLn ("Sua missão mais avancada completada: " ++ (progressoUsuario u))
      iniciarQuizLoop (progressoUsuario u)

novoJogo :: IO ()
novoJogo = do
  sucesso <- atualizaProgresso "1"
  if sucesso
    then do
      putStrLn "Começando novo jogo!"
      iniciarQuizLoop "1"
    else do
      putStrLn "Erro ao reiniciar progresso."
      menuJogo

iniciarQuizLoop :: String -> IO ()
iniciarQuizLoop progresso = do
  putStrLn ""
  putStrLn "Escolha o nível de dificuldade:"
  putStr "Sua escolha: "

  let opcoes = [ "😊 Fácil (pode errar até 3 questões)"
              ,"😉 Médio (pode errar até 2 questões)"
                ,"🤯 Difícil (pode errar até 1 questão)"
                ]
  
  indice <- escolherOpcao "Escolha o nível de dificuldade:" opcoes
  let nivelEscolhido = case indice of
          0 -> Facil
          1 -> Medio
          2 -> Dificil
          _ -> Medio
      
  escolherMissaoMenu progresso nivelEscolhido

filtrarPorIndices :: [Int] -> [a] -> [a]
filtrarPorIndices indices lista =
    [ lista !! (i - 1) | i <- indices, i > 0, i <= length lista ]

escolherMissaoMenu :: String -> Nivel -> IO ()
escolherMissaoMenu progresso nivelEscolhido = do
    let missoesDisponiveis = obterMissoesDisponiveis (progresso)

    let opcoes = filtrarPorIndices missoesDisponiveis missoesMapeadasNomes
    
    indiceMissao <- escolherOpcao "MISSÕES QUE VOCÊ PODE JOGAR BASEADO NO SEU NÍVEL ATUAL:\n" opcoes

    let missaoEscolhida = indiceMissao + 1 

    putStrLn ("\n\nProcessando missão: " ++ show missaoEscolhida)

    executarMissao progresso nivelEscolhido (show missaoEscolhida)

shuffle :: [a] -> IO [a]
shuffle xs = do
    gen <- newStdGen
    return $ shuffle' xs (length xs) gen

executarMissao :: String -> Nivel -> String -> IO ()
executarMissao progresso nivelEscolhido missaoDesejada = do
  todasAsPerguntas <- carregaPerguntas "../data/perguntas.csv"

  let perguntasDaMissao = filter (\p -> missao p == missaoDesejada) todasAsPerguntas

  perguntasEmbaralhadas <- shuffle perguntasDaMissao 

  let perguntasParaExibir = take 10 perguntasEmbaralhadas 

  
  if null perguntasParaExibir
    then do
      putStrLn ("Nenhuma pergunta encontrada para a missao " ++ missaoDesejada ++ ".") 
      menuContinuar "Verifique se o arquivo ../data/perguntas.csv esta correto." progresso
    else do
      let numPerguntas = length perguntasParaExibir
      let largura = larguraTerminal
      putStrLn $ replicate largura '='
      putStrLn $ centralizar largura (" INICIANDO QUIZ DA MISSÃO " ++ missaoDesejada)
      putStrLn $ replicate largura '='
      putStrLn (show numPerguntas ++ " perguntas selecionadas em ordem aleatória")
      putStrLn "Pressione Enter para comecar o quiz..."
      putStrLn ("Nível: " ++ show nivelEscolhido ++ " (máximo " ++ show (maxErrosPermitidos nivelEscolhido) ++ " erros)")
      putStrLn ""
      
      case missaoDesejada of
        "5" -> do 
              putStrLn $ "Batalha dos Fundamentos "
              mostrarLogoCentralizada "../banners/boss1.txt"
        "7" -> do
              putStrLn $ "Guardião da Recursão"
              mostrarLogoCentralizada "../banners/boss2.txt"
        "9" -> do
              putStrLn $ "Mestre da Dedução"
              mostrarLogoCentralizada "../banners/boss3.txt"
        _   -> putStrLn "\nPressione Enter para continuar..."

      _ <- getLine
      
      resultadoMissao <- iniciarQuiz perguntasParaExibir nivelEscolhido missaoDesejada
      
      if resultadoMissao /= "-1" && missaoDesejada == progresso
        then do
          case somaProgresso progresso of
            Just missaoAtual -> do
              atualizaProgresso missaoAtual
              menuContinuar ("\nParabéns 🥳! Você desbloqueou a missão " ++ missaoAtual ++ "🎉!\n") missaoAtual
            Nothing -> do
              putStrLn "Erro: progresso inválido."
        else if resultadoMissao == "-1"
          then do
            _ <- getLine
            menuContinuar "\nVocê falhou na missão 😢. Tente novamente!\n" progresso
          else do
            menuContinuar "\nMissão repetida concluida! 🎈\n" progresso

menuContinuar :: String -> String -> IO ()
menuContinuar titulo progresso = do
    let opcoes =
            [ "🎮 Continuar jogando"
            , "🏠 Voltar ao menu principal"
            , "🚪 Sair"
            ]

    escolha <- escolherOpcao (titulo ++ "======== O QUE DESEJA FAZER? ========") opcoes

    case escolha of
        0 -> iniciarQuizLoop progresso
        1 -> menuJogo
        2 -> putStrLn "Obrigado por jogar!"
        _ -> menuContinuar "" progresso 

somaProgresso :: String -> Maybe String
somaProgresso s = do
  valor <- readMaybe s :: Maybe Int
  let resultado = valor + 1
  return (show resultado)