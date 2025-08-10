module Jogo where

import Missoes 
import Data.List (filter, take)
import Data.Maybe (fromMaybe, isNothing)
import System.CPUTime (getCPUTime)
import Text.Read (readMaybe)  

main :: IO ()
main = do
  putStrLn "Bem vindo!"
  
  putStrLn ""
  putStrLn "=== MENU PRINCIPAL ==="
  putStrLn "1 - Continuar jogo existente"
  putStrLn "2 - Comecar novo jogo"
  putStr "Sua escolha: "
  opcaoMenu <- getLine
  
  case opcaoMenu of
    "1" -> continuarJogo
    "2" -> novoJogo
    _ -> do
      putStrLn "Opcao invalida! Tentando continuar jogo existente..."
      continuarJogo

continuarJogo :: IO ()
continuarJogo = do
  personagem <- carregaPersonagem "personagem.csv"
  
  case personagem of
    Nothing -> do
      putStrLn "Nenhum personagem encontrado. Sera criado um novo jogo."
      novoJogo
    Just p -> do
      putStrLn ("Bem vindo de volta, " ++ nomePersonagem p ++ "!")
      putStrLn ("Sua missao mais avancada completada: " ++ missaoCompletada p)
      iniciarQuizLoop p

novoJogo :: IO ()
novoJogo = do
  putStrLn "=== CRIANDO NOVO PERSONAGEM ==="
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
  putStrLn "1 - Facil (pode errar ate 3 questoes)"
  putStrLn "2 - Medio (pode errar ate 2 questoes)" 
  putStrLn "3 - Dificil (pode errar ate 1 questao)"
  putStr "Sua escolha: "
  nivelInput <- getLine
  
  case stringParaNivel nivelInput of
    Nothing -> do
      putStrLn "Nivel invalido! Usando nivel Medio por padrao."
      iniciarQuizLoop personagemAtual
    Just nivelEscolhido -> do
      putStrLn ("Nivel escolhido: " ++ show nivelEscolhido)
      
      
      escolherMissao personagemAtual nivelEscolhido

escolherMissao :: Personagem -> Nivel -> IO ()
escolherMissao personagemAtual nivelEscolhido = do
  let missoesDisponiveis = obterMissoesDisponiveis (missaoCompletada personagemAtual)
  putStrLn ""
  putStrLn "========================================="
  putStrLn "           ESCOLHA DA MISSAO"
  putStrLn "========================================="
  putStrLn "Missoes que voce pode jogar:"
  mapM_ (\m -> putStrLn ("  -> Missao " ++ m)) missoesDisponiveis
  putStrLn ""
  putStrLn "INSTRUCOES:"
  putStrLn "- Digite APENAS o numero da missao"
  putStrLn "- Exemplos validos: 1, 2, 3, etc."
  putStrLn "- Nao digite 'Missao 1' ou outros textos"
  putStrLn ""
  putStr "Digite o numero da missao desejada: "
  
  missaoInput <- getLine
  let missaoLimpa = filter (/= ' ') missaoInput  
  
  putStrLn ("Processando missao: " ++ missaoLimpa)
  
  case readMaybe missaoLimpa :: Maybe Int of
    Nothing -> do
      putStrLn ""
      putStrLn "ERRO: Por favor digite apenas um NUMERO!"
      putStrLn "Exemplo: se quer jogar a missao 1, digite: 1"
      putStrLn ""
      putStr "Pressione Enter para tentar novamente..."
      _ <- getLine
      escolherMissao personagemAtual nivelEscolhido
      
    Just numeroMissao -> do
      case readMaybe (missaoCompletada personagemAtual) :: Maybe Int of
        Nothing -> do
          putStrLn "ERRO: Problema com dados do personagem!"
          menuContinuar personagemAtual
        Just maxMissao -> do
          if numeroMissao < 1 then do
            putStrLn ""import missoes
import Data.List (filter, take)
import Data.Maybe (fromMaybe, isNothing)
import System.CPUTime (getCPUTime)
import Text.Read (readMaybe)  

main :: IO ()
main = do
  putStrLn "Bem vindo!"
  
  putStrLn ""
  putStrLn "=== MENU PRINCIPAL ==="
  putStrLn "1 - Continuar jogo existente"
  putStrLn "2 - Comecar novo jogo"
  putStr "Sua escolha: "
  opcaoMenu <- getLine
  
  case opcaoMenu of
    "1" -> continuarJogo
    "2" -> novoJogo
    _ -> do
      putStrLn "Opcao invalida! Tentando continuar jogo existente..."
      continuarJogo

continuarJogo :: IO ()
continuarJogo = do
  personagem <- carregaPersonagem "personagem.csv"
  
  case personagem of
    Nothing -> do
      putStrLn "Nenhum personagem encontrado. Sera criado um novo jogo."
      novoJogo
    Just p -> do
      putStrLn ("Bem vindo de volta, " ++ nomePersonagem p ++ "!")
      putStrLn ("Sua missao mais avancada completada: " ++ missaoCompletada p)
      iniciarQuizLoop p

novoJogo :: IO ()
novoJogo = do
  putStrLn "=== CRIANDO NOVO PERSONAGEM ==="
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
  putStrLn "1 - Facil (pode errar ate 3 questoes)"
  putStrLn "2 - Medio (pode errar ate 2 questoes)" 
  putStrLn "3 - Dificil (pode errar ate 1 questao)"
  putStr "Sua escolha: "
  nivelInput <- getLine
  
  case stringParaNivel nivelInput of
    Nothing -> do
      putStrLn "Nivel invalido! Usando nivel Medio por padrao."
      iniciarQuizLoop personagemAtual
    Just nivelEscolhido -> do
      putStrLn ("Nivel escolhido: " ++ show nivelEscolhido)
      
      
      escolherMissao personagemAtual nivelEscolhido

escolherMissao :: Personagem -> Nivel -> IO ()
escolherMissao personagemAtual nivelEscolhido = do
  let missoesDisponiveis = obterMissoesDisponiveis (missaoCompletada personagemAtual)
  putStrLn ""
  putStrLn "========================================="
  putStrLn "           ESCOLHA DA MISSAO"
  putStrLn "========================================="
  putStrLn "Missoes que voce pode jogar:"
  mapM_ (\m -> putStrLn ("  -> Missao " ++ m)) missoesDisponiveis
  putStrLn ""
  putStrLn "INSTRUCOES:"
  putStrLn "- Digite APENAS o numero da missao"
  putStrLn "- Exemplos validos: 1, 2, 3, etc."
  putStrLn "- Nao digite 'Missao 1' ou outros textos"
  putStrLn ""
  putStr "Digite o numero da missao desejada: "
  
  missaoInput <- getLine
  let missaoLimpa = filter (/= ' ') missaoInput  
  
  putStrLn ("Processando missao: " ++ missaoLimpa)
  
  case readMaybe missaoLimpa :: Maybe Int of
    Nothing -> do
      putStrLn ""
      putStrLn "ERRO: Por favor digite apenas um NUMERO!"
      putStrLn "Exemplo: se quer jogar a missao 1, digite: 1"
      putStrLn ""
      putStr "Pressione Enter para tentar novamente..."
      _ <- getLine
      escolherMissao personagemAtual nivelEscolhido
      
    Just numeroMissao -> do
      case readMaybe (missaoCompletada personagemAtual) :: Maybe Int of
        Nothing -> do
          putStrLn "ERRO: Problema com dados do personagem!"
          menuContinuar personagemAtual
        Just maxMissao -> do
          if numeroMissao < 1 then do
            putStrLn ""
            putStrLn "ERRO: O numero da missao deve ser maior que 0!"
            putStr "Pressione Enter para tentar novamente..."
            _ <- getLine
            escolherMissao personagemAtual nivelEscolhido
          else if numeroMissao > maxMissao then do
            putStrLn ""
            putStrLn ("ERRO: Voce so pode jogar missoes de 1 ate " ++ show maxMissao)
            putStrLn ("Voce escolheu a missao " ++ show numeroMissao ++ " que ainda nao esta disponivel!")
            putStr "Pressione Enter para tentar novamente..."
            _ <- getLine
            escolherMissao personagemAtual nivelEscolhido
          else do
            
            executarMissao personagemAtual nivelEscolhido (show numeroMissao)

executarMissao :: Personagem -> Nivel -> String -> IO ()
executarMissao personagemAtual nivelEscolhido missaoDesejada = do
  todasAsPerguntas <- carregaPerguntas "quiz_completo.csv"
  
  let perguntasDaMissao = filter (\p -> missao p == missaoDesejada) todasAsPerguntas
  
  
  let perguntasParaExibir = take 10 perguntasDaMissao
  
  if null perguntasParaExibir
    then do
      putStrLn ("Nenhuma pergunta encontrada para a missao " ++ missaoDesejada ++ ".")
      putStrLn "Verifique se o arquivo quiz_completo.csv esta correto."
      menuContinuar personagemAtual
    else do
      let numPerguntas = length perguntasParaExibir
      putStrLn ("\n--- INICIANDO QUIZ DA MISSAO " ++ missaoDesejada ++ " ---")
      putStrLn (show numPerguntas ++ " perguntas selecionadas em ordem sequencial")
      putStr "Pressione Enter para comecar o quiz..."
      putStrLn ("Nivel: " ++ show nivelEscolhido ++ " (maximo " ++ show (maxErrosPermitidos nivelEscolhido) ++ " erros)")
      putStrLn ""
      
      _ <- getLine
      
      resultadoMissao <- iniciarQuiz perguntasParaExibir nivelEscolhido missaoDesejada
      
      if resultadoMissao /= "-1" && missaoDesejada == missaoCompletada personagemAtual
        then do
          let novaMissaoCompletada = atualizarProgressoPersonagem missaoDesejada (missaoCompletada personagemAtual)
          let personagemAtualizado = personagemAtual { missaoCompletada = novaMissaoCompletada }
          salvarPersonagem "personagem.csv" personagemAtualizado
          putStrLn ("\nParabens! Voce desbloqueou a missao " ++ novaMissaoCompletada ++ "!")
          menuContinuar personagemAtualizado
        else if resultadoMissao == "-1"
          then do
            putStrLn "\nVoce falhou na missao. Tente novamente!"
            menuContinuar personagemAtual
          else do
            putStrLn "\nMissao repetida concluida!"
            menuContinuar personagemAtual

menuContinuar :: Personagem -> IO ()
menuContinuar personagem = do
  putStrLn "\n=== O QUE DESEJA FAZER? ==="
  putStrLn "1 - Continuar jogando"
  putStrLn "2 - Voltar ao menu principal"
  putStrLn "3 - Sair do jogo"
  putStr "Sua escolha: "
  opcao <- getLine
  
  case opcao of
    "1" -> iniciarQuizLoop personagem
    "2" -> main
    "3" -> putStrLn "Obrigado por jogar!"
    _ -> do
      putStrLn "Opcao invalida!"
      menuContinuar personagem

            putStrLn "ERRO: O numero da missao deve ser maior que 0!"
            putStr "Pressione Enter para tentar novamente..."
            _ <- getLine
            escolherMissao personagemAtual nivelEscolhido
          else if numeroMissao > maxMissao then do
            putStrLn ""
            putStrLn ("ERRO: Voce so pode jogar missoes de 1 ate " ++ show maxMissao)
            putStrLn ("Voce escolheu a missao " ++ show numeroMissao ++ " que ainda nao esta disponivel!")
            putStr "Pressione Enter para tentar novamente..."
            _ <- getLine
            escolherMissao personagemAtual nivelEscolhido
          else do
            
            executarMissao personagemAtual nivelEscolhido (show numeroMissao)

executarMissao :: Personagem -> Nivel -> String -> IO ()
executarMissao personagemAtual nivelEscolhido missaoDesejada = do
  todasAsPerguntas <- carregaPerguntas "quiz_completo.csv"
  
  let perguntasDaMissao = filter (\p -> missao p == missaoDesejada) todasAsPerguntas
  
  
  let perguntasParaExibir = take 10 perguntasDaMissao
  
  if null perguntasParaExibir
    then do
      putStrLn ("Nenhuma pergunta encontrada para a missao " ++ missaoDesejada ++ ".")
      putStrLn "Verifique se o arquivo quiz_completo.csv esta correto."
      menuContinuar personagemAtual
    else do
      let numPerguntas = length perguntasParaExibir
      putStrLn ("\n--- INICIANDO QUIZ DA MISSAO " ++ missaoDesejada ++ " ---")
      putStrLn (show numPerguntas ++ " perguntas selecionadas em ordem sequencial")
      putStr "Pressione Enter para comecar o quiz..."
      putStrLn ("Nivel: " ++ show nivelEscolhido ++ " (maximo " ++ show (maxErrosPermitidos nivelEscolhido) ++ " erros)")
      putStrLn ""
      
      _ <- getLine
      
      resultadoMissao <- iniciarQuiz perguntasParaExibir nivelEscolhido missaoDesejada
      
      if resultadoMissao /= "-1" && missaoDesejada == missaoCompletada personagemAtual
        then do
          let novaMissaoCompletada = atualizarProgressoPersonagem missaoDesejada (missaoCompletada personagemAtual)
          let personagemAtualizado = personagemAtual { missaoCompletada = novaMissaoCompletada }
          salvarPersonagem "personagem.csv" personagemAtualizado
          putStrLn ("\nParabens! Voce desbloqueou a missao " ++ novaMissaoCompletada ++ "!")
          menuContinuar personagemAtualizado
        else if resultadoMissao == "-1"
          then do
            putStrLn "\nVoce falhou na missao. Tente novamente!"
            menuContinuar personagemAtual
          else do
            putStrLn "\nMissao repetida concluida!"
            menuContinuar personagemAtual

menuContinuar :: Personagem -> IO ()
menuContinuar personagem = do
  putStrLn "\n=== O QUE DESEJA FAZER? ==="
  putStrLn "1 - Continuar jogando"
  putStrLn "2 - Voltar ao menu principal"
  putStrLn "3 - Sair do jogo"
  putStr "Sua escolha: "
  opcao <- getLine
  
  case opcao of
    "1" -> iniciarQuizLoop personagem
    "2" -> main
    "3" -> putStrLn "Obrigado por jogar!"
    _ -> do
      putStrLn "Opcao invalida!"
      menuContinuar personagem
