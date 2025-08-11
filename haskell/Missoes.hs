module Missoes where

import System.IO
import Data.List (isPrefixOf, take, filter)
import Data.Char (toLower)
import Text.Read (readMaybe)  
import Utils (larguraTerminal, limparTela )
import Usuario


data Personagem = Personagem
  { nomePersonagem :: String
--   , senhaUsuario :: String
  , missaoCompletada :: String
  } deriving (Show)

data Pergunta = Pergunta
  { idQuestao :: String
  , missao :: String
  , textoPergunta :: String
  , alternativa_Certa :: String
  , texto_alternativa_a :: String
  , texto_alternativa_b :: String
  , texto_alternativa_c :: String
  , texto_alternativa_d :: String
  , texto_alternativa_e :: String
  } deriving (Show)

data Nivel = Facil | Medio | Dificil deriving (Show, Eq)

data ResultadoQuiz = ResultadoQuiz
  { acertos :: [Pergunta]
  , erros :: [Pergunta]
  , nivel :: Nivel
  } deriving (Show)

carregaPerguntas :: FilePath -> IO [Pergunta]
carregaPerguntas caminho = do
    conteudo <- readFile caminho
    let linhas = tail (lines conteudo)
    return (map parseLinha linhas)

carregaPersonagem :: FilePath -> IO (Maybe Personagem)
carregaPersonagem caminho = do
    existe <- doesFileExist caminho
    if not existe
        then return Nothing
        else do
            conteudo <- readFile caminho
            let linhas = lines conteudo
            if length linhas >= 2
                then do
                    let dadosLinha = tail linhas !! 0
                    let colunas = map removeAspas (splitOn ',' dadosLinha)
                    if length colunas >= 2
                        then return $ Just $ Personagem (colunas !! 0) (colunas !! 1)
                        else return Nothing
                else return Nothing

doesFileExist :: FilePath -> IO Bool
doesFileExist _ = return True 

salvarPersonagem :: FilePath -> Personagem -> IO ()
salvarPersonagem caminho personagem = do
    let cabecalho = "nome,Missao_completada"
    let dadosPersonagem = nomePersonagem personagem ++ "," ++ missaoCompletada personagem
    writeFile caminho (cabecalho ++ "\n" ++ dadosPersonagem ++ "\n")

-- removeAspas :: String -> String
-- removeAspas str
--   | length str >= 2 && head str == '"' && last str == '"' = init (tail str)
--   | otherwise = str

parseLinha :: String -> Pergunta
parseLinha linha =
  let colunas = map removeAspas (splitOn ',' linha)
  in Pergunta
    { idQuestao = colunas !! 0
    , missao = colunas !! 1
    , textoPergunta = colunas !! 2
    , alternativa_Certa = colunas !! 3
    , texto_alternativa_a = colunas !! 4
    , texto_alternativa_b = colunas !! 5
    , texto_alternativa_c = colunas !! 6
    , texto_alternativa_d = colunas !! 7
    , texto_alternativa_e = colunas !! 8
    }

-- splitOn :: Char -> String -> [String]
-- splitOn delimiter = foldr f [[]]
--   where
--     f c (h:t)
--       | c == delimiter = []:h:t
--       | otherwise = (c:h):t

exibirPergunta :: Pergunta -> IO ()
exibirPergunta p = do
    putStrLn $ replicate larguraTerminal '-'
    putStrLn ("ID: " ++ idQuestao p)
    putStrLn ("Missao: " ++ missao p)
    putStrLn ("Pergunta: ")
    putStrLn (textoPergunta p)
    putStrLn ("a) " ++ texto_alternativa_a p)
    putStrLn ("b) " ++ texto_alternativa_b p)
    putStrLn ("c) " ++ texto_alternativa_c p)
    putStrLn ("d) " ++ texto_alternativa_d p)
    putStrLn ("e) " ++ texto_alternativa_e p)
    putStrLn $ replicate larguraTerminal '-'


normalizaResposta :: String -> String
normalizaResposta = map toLower . filter (/= ' ')

compara_acertou_errou :: String -> String -> Bool
compara_acertou_errou respostaUsuario alternativaCorreta =
    normalizaResposta respostaUsuario == normalizaResposta alternativaCorreta

perguntaParaCSV :: Pergunta -> String
perguntaParaCSV p = 
    idQuestao p ++ "," ++
    missao p ++ "," ++
    textoPergunta p ++ "," ++
    alternativa_Certa p ++ "," ++
    texto_alternativa_a p ++ "," ++
    texto_alternativa_b p ++ "," ++
    texto_alternativa_c p ++ "," ++
    texto_alternativa_d p ++ "," ++
    texto_alternativa_e p

salvarRespostaAcertada :: Pergunta -> IO ()
salvarRespostaAcertada pergunta = do
    appendFile "respostasAcertadas.csv" (perguntaParaCSV pergunta ++ "\n")

stringParaNivel :: String -> Maybe Nivel
stringParaNivel s
  | normalizaResposta s == "facil" || s == "1" = Just Facil
  | normalizaResposta s == "medio" || s == "2" = Just Medio
  | normalizaResposta s == "dificil" || s == "3" = Just Dificil
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
    
    putStrLn ("Nível escolhido: " ++ show (nivel resultado))
    putStrLn ("Total de acertos: " ++ show (length (acertos resultado)))
    putStrLn ("Total de erros: " ++ show (length (erros resultado)))
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
            putStrLn "PERGUNTAS QUE VOCE ERROU:"
            putStrLn $ replicate larguraTerminal '-'
            mapM_ exibirResumoPerguntas (reverse (erros resultado))
        else putStrLn "Parabens! Voce nao errou nenhuma pergunta!"
    
exibirResumoPerguntas :: Pergunta -> IO ()
exibirResumoPerguntas p = do
    putStrLn ("ID: " ++ idQuestao p)
    putStrLn ("Pergunta: " ++ textoPergunta p)
    putStrLn ("Resposta correta: " ++ alternativa_Certa p)
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

atualizarProgressoPersonagem :: String -> String -> String
atualizarProgressoPersonagem missaoAtual missaoCompletada =
    case (readMaybe missaoAtual :: Maybe Int, readMaybe missaoCompletada :: Maybe Int) of
        (Just missaoAtualNum, Just missaoCompletadaNum) ->
            let proximaMissao = missaoAtualNum + 1
            in if missaoAtualNum == missaoCompletadaNum
               then show proximaMissao
               else missaoCompletada
        _ -> missaoCompletada

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
            putStrLn ("GAME OVER! Voce excedeu o limite de erros para o nivel " ++ show (nivel resultado))
            putStrLn ("Limite de erros: " ++ show (maxErrosPermitidos (nivel resultado)))
            return resultado
        else do
            limparTela
            putStrLn ("Acertos: " ++ show (length (acertos resultado)) ++ " | Erros: " ++ show numErros ++ "/" ++ show (maxErrosPermitidos (nivel resultado)))
            putStrLn ""
            exibirPergunta h
            
            putStr "Escolha uma alternativa: "
            alternativa_usuario <- getLine
            
            let comparacao = compara_acertou_errou alternativa_usuario (alternativa_Certa h)
            
            if comparacao
                then do
                    putStrLn "Parabéns! Você acertou!!!"
                    salvarRespostaAcertada h
                    let novoResultado = resultado { acertos = h : acertos resultado }
                    putStrLn "Pressione ENTER para continuar..."
                    _ <- getLine
                    executarQuiz t novoResultado
                else do
                    putStrLn ("Ops... não foi dessa vez! A resposta correta era: " ++ alternativa_Certa h)
                    let novoResultado = resultado { erros = h : erros resultado }
                    putStrLn "Pressione ENTER para continuar..."
                    _ <- getLine
                    executarQuiz t novoResultado