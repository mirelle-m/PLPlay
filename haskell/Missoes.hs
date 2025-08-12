module Missoes where

import System.IO (appendFile)
import Data.Char (toLower)
import Text.Read (readMaybe)  
import Utils
import Usuario
import Data.List.Split (splitOn)
import Data.Function (on)
import Data.List (intercalate)

data Pergunta = Pergunta
  { id_questao :: String,
    missao :: String,
    pergunta :: String,
    alternativa_certa :: String,
    texto_alternativa_a :: String,
    texto_alternativa_b :: String,
    texto_alternativa_c :: String,
    texto_alternativa_d :: String,
    texto_alternativa_e :: String
  } deriving (Show)

data Nivel = Facil | Medio | Dificil deriving (Show, Eq)

data ResultadoQuiz = ResultadoQuiz
  { acertos :: [Pergunta],
    erros :: [Pergunta],
    nivel :: Nivel
  } deriving (Show)

carregaPerguntas :: FilePath -> IO [Pergunta]
carregaPerguntas caminho = do
    conteudo <- readFile caminho
    let linhas = tail (lines conteudo)
    return (map parseLinha linhas)


parseLinha :: String -> Pergunta
parseLinha linha =
  let colunas = map removeAspas (splitOn ";" linha)
  in Pergunta
    { id_questao = colunas !! 0,
      missao = colunas !! 1,
      pergunta = colunas !! 2,
      alternativa_certa = colunas !! 3,
      texto_alternativa_a = colunas !! 4,
      texto_alternativa_b = colunas !! 5,
      texto_alternativa_c = colunas !! 6,
      texto_alternativa_d = colunas !! 7,
      texto_alternativa_e = colunas !! 8
    }


exibirPergunta :: Pergunta -> IO ()
exibirPergunta p = do
    putStrLn $ replicate larguraTerminal '-'
    putStrLn ("ID: " ++ id_questao p)
    putStrLn ("Missao: " ++ missao p)
    putStrLn ("Pergunta: " ++ pergunta p)
    putStrLn ("a) " ++ texto_alternativa_a p)
    putStrLn ("b) " ++ texto_alternativa_b p)
    putStrLn ("c) " ++ texto_alternativa_c p)
    putStrLn ("d) " ++ texto_alternativa_d p)
    putStrLn ("e) " ++ texto_alternativa_e p)
    putStrLn $ replicate larguraTerminal '-'

textoRespostaCorreta :: Pergunta -> String
textoRespostaCorreta p = case map toLower (alternativa_certa p) of
  "a" -> texto_alternativa_a p
  "b" -> texto_alternativa_b p
  "c" -> texto_alternativa_c p
  "d" -> texto_alternativa_d p
  "e" -> texto_alternativa_e p
  _   -> "Alternativa correta inválida"


normalizarResposta :: String -> String
normalizarResposta = map toLower . filter (/= ' ')


compararAcertouErrou :: String -> String -> Bool
compararAcertouErrou = (==) `on` normalizarResposta


transformarEmCSV :: Pergunta -> String
transformarEmCSV p =
    intercalate ";" $ map adicionaAspas
      [ id_questao p,
        missao p,
        pergunta p,
        alternativa_certa p,
        texto_alternativa_a p,
        texto_alternativa_b p,
        texto_alternativa_c p,
        texto_alternativa_d p,
        texto_alternativa_e p
      ]


transformarEmFlashcard :: Pergunta -> String
transformarEmFlashcard p =
  intercalate ";" $ map adicionaAspas
    [ pergunta p, textoRespostaCorreta p ]


salvarRespostaAcertada :: Pergunta -> IO ()
salvarRespostaAcertada pergunta =
    appendFile "../data/acertos.csv" ("\n" ++ transformarEmCSV pergunta)


salvarQuestoesParaTreino :: Pergunta -> IO ()
salvarQuestoesParaTreino pergunta =
    appendFile "../data/flashcards.csv" ("\n" ++ transformarEmFlashcard pergunta)


stringParaNivel :: String -> Maybe Nivel
stringParaNivel s
  | normalizarResposta s == "facil" || s == "1" = Just Facil
  | normalizarResposta s == "medio" || s == "2" = Just Medio
  | normalizarResposta s == "dificil" || s == "3" = Just Dificil
  | otherwise = Nothing


maxErrosPermitidos :: Nivel -> Int
maxErrosPermitidos Facil = 3
maxErrosPermitidos Medio = 2
maxErrosPermitidos Dificil = 1


podeContinuar :: Int -> Nivel -> Bool
podeContinuar numErros nivelEscolhido = numErros < maxErrosPermitidos nivelEscolhido


exibirResumo :: ResultadoQuiz -> IO ()
exibirResumo resultado = do
    limparTela
    let numErros = length (erros resultado)
    let nivelAtual = nivel resultado
    if numErros >= maxErrosPermitidos nivelAtual
        then do
            putStrLn $ replicate larguraTerminal '-'
            putStrLn "            GAME OVER!            "
            putStrLn $ replicate larguraTerminal '-'
            putStrLn ("Você excedeu o limite de erros para o nível " ++ show nivelAtual)
            putStrLn ("Limite de erros: " ++ show (maxErrosPermitidos nivelAtual))
        else do
            putStrLn $ replicate larguraTerminal '-'
            putStrLn "        MISSÃO CONCLUIDA!         "
            putStrLn $ replicate larguraTerminal '-'
    putStrLn ("Nível escolhido: " ++ show nivelAtual)
    putStrLn ("Total de acertos: " ++ show (length (acertos resultado)))
    putStrLn ("Total de erros: " ++ show numErros)
    putStrLn ""
    if not (null (acertos resultado))
        then do
            putStrLn "PERGUNTAS QUE VOCÊ ACERTOU:"
            putStrLn $ replicate larguraTerminal '-'
            mapM_ exibirResumoPerguntas (reverse (acertos resultado))
            putStrLn ""
        else putStrLn "Nenhuma pergunta foi acertada.\n"
    if not (null (erros resultado))
        then do
            putStrLn "PERGUNTAS QUE VOCÊ ERROU:"
            putStrLn $ replicate larguraTerminal '-'
            mapM_ exibirResumoPerguntas (reverse (erros resultado))
        else putStrLn "Parabéns! Você não errou nenhuma pergunta!"


exibirResumoPerguntas :: Pergunta -> IO ()
exibirResumoPerguntas p = do
    putStrLn ("ID: " ++ id_questao p)
    putStrLn ("Pergunta: " ++ pergunta p)
    putStrLn ("Resposta correta: " ++ alternativa_certa p)
    putStrLn "---"


verificarMissaoValida :: String -> String -> Bool
verificarMissaoValida missaoEscolhida missaoCompletadaAtual =
    let missaoLimpa = filter (/= ' ') missaoEscolhida  
    in case (readMaybe missaoLimpa :: Maybe Int, readMaybe missaoCompletadaAtual :: Maybe Int) of
        (Just escolhida, Just completada) -> escolhida >= 1 && escolhida <= completada
        _ -> False


obterMissoesDisponiveis :: String -> [Int]
obterMissoesDisponiveis missaoCompletada =
    case readMaybe missaoCompletada :: Maybe Int of
        Just missaoCompletadaNum -> [1 .. missaoCompletadaNum]
        Nothing                  -> [1]


iniciarQuiz :: [Pergunta] -> Nivel -> String -> IO String
iniciarQuiz perguntas nivelEscolhido missaoAtual = do
    putStrLn "Iniciando o quiz..."
    resultado <- executarQuiz perguntas (ResultadoQuiz [] [] nivelEscolhido)
    exibirResumo resultado
    let numErros = length (erros resultado)
    let nivelAtual = nivel resultado
    if numErros < maxErrosPermitidos nivelAtual && not (null perguntas)
        then return missaoAtual  
        else return "-1"  


executarQuiz :: [Pergunta] -> ResultadoQuiz -> IO ResultadoQuiz
executarQuiz [] resultado = return resultado
executarQuiz (h:t) resultado = do
    let numErros = length (erros resultado)
    if not (podeContinuar numErros (nivel resultado))
        then do
            limparTela
            putStrLn ("GAME OVER! Você excedeu o limite de erros para o nível " ++ show (nivel resultado))
            putStrLn ("Limite de erros: " ++ show (maxErrosPermitidos (nivel resultado)))
            return resultado
        else do
            limparTela
            putStrLn ("Acertos: " ++ show (length (acertos resultado)) ++
                     " | Erros: " ++ show numErros ++ "/" ++ show (maxErrosPermitidos (nivel resultado)))
            putStrLn ""
            exibirPergunta h
            alternativa_usuario <- captura_alternativa
            let comparacao = compararAcertouErrou alternativa_usuario (alternativa_certa h)
            if comparacao
                then do
                    putStrLn "Parabéns! Você acertou!!!"
                    salvarRespostaAcertada h
                    let novoResultado = resultado { acertos = h : acertos resultado }
                    opcaoSalvarTreino h t novoResultado
                else do
                    putStrLn ("Ops... não foi dessa vez! A resposta correta era: " ++ alternativa_certa h)
                    let novoResultado = resultado { erros = h : erros resultado }
                    opcaoSalvarTreino h t novoResultado


captura_alternativa :: IO String
captura_alternativa = do
    let validas = ["a", "b", "c", "d", "e"]
    putStr "Escolha uma alternativa (a, b, c, d, e): "
    alternativa <- getLine
    if alternativa `elem` validas
        then return alternativa
        else do
            putStrLn "Opção inválida. Tente novamente.\n"
            captura_alternativa


missoesMapeadasNomes :: [String]
missoesMapeadasNomes = ["🧭 Missão 1: Introdução - Históricos e Características"
      ,"🧭 Missão 2: Classificação e Características"
      ,"🧭 Missão 3: Valores, Tipos e Sistema de Tipos"
      ,"🧭 Missão 4: Paradigma Imperativo"
      ,"👾 Chefão 5: Batalha dos Fundamentos"
      ,"🧭 Missão 6: Paradigma Funcional"
      ,"👾 Chefão 7: Guardião da Recursão"
      ,"🧭 Missão 8: Paradigma Lógico"
      ,"👾 Chefão 9: Mestre da Dedução"]


imprimirMapa :: IO ()
imprimirMapa = do
  mostrarLogoCentralizada "../banners/mapa.txt"


opcaoSalvarTreino :: Pergunta -> [Pergunta] -> ResultadoQuiz -> IO ResultadoQuiz
opcaoSalvarTreino pergunta restoPerguntas resultadoAtual = do
    putStrLn $ replicate larguraTerminal '-'
    putStrLn "Selecione uma opção:"
    putStrLn " 1 - Salvar pergunta para treino e continuar"
    putStrLn " 2 - Continuar\n"
    opcao <- getLine
    case opcao of
      "1" -> do
        salvarQuestoesParaTreino pergunta
        executarQuiz restoPerguntas resultadoAtual
      _   -> executarQuiz restoPerguntas resultadoAtual