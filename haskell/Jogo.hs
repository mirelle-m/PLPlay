module Jogo where
import Missoes
import Data.List (filter, take)
import Data.Maybe (fromMaybe, isNothing)
import System.CPUTime (getCPUTime)
import Text.Read (readMaybe)  
import Navegacao(escolherOpcao,escolherOpcaoComTitulo)
import MapaMissoes
import Utils (carregarLogo, centralizar, limparTela, larguraTerminal)

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
      putStrLn "Opcao invalida! Tentando continuar jogo existente..."
      continuarJogo

continuarJogo :: IO ()
continuarJogo = do
  personagem <- carregaPersonagem "personagem.csv"
  
  case personagem of
    Nothing -> do
      putStrLn "Nenhum personagem encontrado. Será criado um novo jogo."
      novoJogo
    Just p -> do
      putStrLn ("Bem vindo de volta, " ++ nomePersonagem p ++ "!")
      putStrLn ("Sua missao mais avancada completada: " ++ missaoCompletada p)
      iniciarQuizLoop p

novoJogo :: IO ()
novoJogo = do
  let largura = larguraTerminal
  putStrLn $ replicate largura '='
  putStrLn $ centralizar largura " CRIANDO NOVO PERSONAGEM "
  putStrLn $ replicate largura '='
  putStr "Digite o nome do seu personagem: "
  nome <- getLine
  let novoPersonagem = Personagem nome "1"
  salvarPersonagem "personagem.csv" novoPersonagem
  putStrLn ("Personagem " ++ nome ++ " criado! Comecando na missao 1.")
  iniciarQuizLoop novoPersonagem

iniciarQuizLoop :: Personagem -> IO ()
iniciarQuizLoop personagemAtual = do
  putStrLn ""
  putStrLn "Escolha o nivel de dificuldade:"
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
      
  escolherMissaoMenu personagemAtual nivelEscolhido

filtrarPorIndices :: [Int] -> [a] -> [a]
filtrarPorIndices indices lista =
    [ lista !! (i - 1) | i <- indices, i > 0, i <= length lista ]

escolherMissaoMenu :: Personagem -> Nivel -> IO ()
escolherMissaoMenu personagemAtual nivelEscolhido = do
    let missoesDisponiveis = obterMissoesDisponiveis (missaoCompletada personagemAtual)

    let opcoes = filtrarPorIndices missoesDisponiveis missoesMapeadasNomes
    -- Seleção por setas
    indiceMissao <- escolherOpcao "MISSÕES QUE VOCÊ PODE JOGAR BASEADO NO SEU NÍVEL ATUAL:\n" opcoes

    let missaoEscolhida = indiceMissao + 1 -- índice começa em 0, missões começam em 1

    putStrLn ("\n\nProcessando missão: " ++ show missaoEscolhida)

    -- Chama a função que executa a missão
    executarMissao personagemAtual nivelEscolhido (show missaoEscolhida)

executarMissao :: Personagem -> Nivel -> String -> IO ()
executarMissao personagemAtual nivelEscolhido missaoDesejada = do
  todasAsPerguntas <- carregaPerguntas "quiz_completo.csv"
  
  let perguntasDaMissao = filter (\p -> missao p == missaoDesejada) todasAsPerguntas
  
  let perguntasParaExibir = take 10 perguntasDaMissao
  
  if null perguntasParaExibir
    then do
      putStrLn ("Nenhuma pergunta encontrada para a missao " ++ missaoDesejada ++ ".") 
      menuContinuar "Verifique se o arquivo quiz_completo.csv esta correto." personagemAtual
    else do
      let numPerguntas = length perguntasParaExibir
      let largura = larguraTerminal
      putStrLn $ replicate largura '='
      putStrLn $ centralizar largura (" INICIANDO QUIZ DA MISSÃO " ++ missaoDesejada)
      putStrLn $ replicate largura '='
      -- putStrLn ("\n--- INICIANDO QUIZ DA MISSÃO " ++ missaoDesejada ++ " ---")
      putStrLn (show numPerguntas ++ " perguntas selecionadas em ordem sequencial")
      putStrLn "Pressione Enter para comecar o quiz..."
      putStrLn ("Nível: " ++ show nivelEscolhido ++ " (máximo " ++ show (maxErrosPermitidos nivelEscolhido) ++ " erros)")
      putStrLn ""
      
      _ <- getLine
      
      resultadoMissao <- iniciarQuiz perguntasParaExibir nivelEscolhido missaoDesejada
      
      if resultadoMissao /= "-1" && missaoDesejada == missaoCompletada personagemAtual
        then do
          let novaMissaoCompletada = atualizarProgressoPersonagem missaoDesejada (missaoCompletada personagemAtual)
          let personagemAtualizado = personagemAtual { missaoCompletada = novaMissaoCompletada }
          salvarPersonagem "personagem.csv" personagemAtualizado
          menuContinuar ("\nParabéns 🥳! Você desbloqueou a missão " ++ novaMissaoCompletada ++ "🎉!\n") personagemAtualizado
        else if resultadoMissao == "-1"
          then do
            _ <- getLine
            menuContinuar "\nVocê falhou na missão 😢. Tente novamente!\n" personagemAtual
          else do
            menuContinuar "\nMissão repetida concluida! 🎈\n" personagemAtual

menuContinuar :: String -> Personagem -> IO ()
menuContinuar titulo personagem = do
    let opcoes =
            [ "🎮 Continuar jogando"
            , "🏠 Voltar ao menu principal"
            , "🚪 Sair"
            ]

    escolha <- escolherOpcao (titulo ++ "======== O QUE DESEJA FAZER? ========") opcoes

    case escolha of
        0 -> iniciarQuizLoop personagem
        1 -> menuJogo
        2 -> putStrLn "Obrigado por jogar!"
        _ -> menuContinuar "" personagem 
