import System.IO
import Data.List (isPrefixOf, take, filter)
import Data.Char (toLower)

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

limparTela :: IO ()
limparTela = putStr (replicate 50 '\n')

removeAspas :: String -> String
removeAspas str
  | length str >= 2 && head str == '"' && last str == '"' = init (tail str)
  | otherwise = str

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

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c (x:xs)
      | c == delimiter = []:x:xs
      | otherwise = (c:x):xs

exibirPergunta :: Pergunta -> IO ()
exibirPergunta p = do
    putStrLn "----------------------------Escolha uma alternativa----------------------------"
    putStrLn ("ID: " ++ idQuestao p)
    putStrLn ("Missao: " ++ missao p)
    putStrLn ("Pergunta: ")
    putStrLn (textoPergunta p)
    putStrLn ("a: " ++ texto_alternativa_a p)
    putStrLn ("b: " ++ texto_alternativa_b p)
    putStrLn ("c: " ++ texto_alternativa_c p)
    putStrLn ("d: " ++ texto_alternativa_d p)
    putStrLn ("e: " ++ texto_alternativa_e p)
    putStrLn "----------------------------Escolha uma alternativa----------------------------"

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
            putStrLn "===================================================================="
            putStrLn "            GAME OVER!            "
            putStrLn "===================================================================="
            putStrLn ("Voce excedeu o limite de erros para o nivel " ++ show nivelAtual)
            putStrLn ("Limite de erros: " ++ show (maxErrosPermitidos nivelAtual))
        else do
            putStrLn "===================================================================="
            putStrLn "        MISSAO CONCLUIDA!         "
            putStrLn "===================================================================="
    
    putStrLn ("Nivel escolhido: " ++ show (nivel resultado))
    putStrLn ("Total de acertos: " ++ show (length (acertos resultado)))
    putStrLn ("Total de erros: " ++ show (length (erros resultado)))
    putStrLn ""
    
    if not (null (acertos resultado))
        then do
            putStrLn "PERGUNTAS QUE VOCE ACERTOU:"
            putStrLn "----------------------------"
            mapM_ exibirResumoPerguntas (reverse (acertos resultado))
            putStrLn ""
        else putStrLn "Nenhuma pergunta foi acertada.\n"
    
    if not (null (erros resultado))
        then do
            putStrLn "PERGUNTAS QUE VOCE ERROU:"
            putStrLn "-------------------------"
            mapM_ exibirResumoPerguntas (reverse (erros resultado))
        else putStrLn "Parabens! Voce nao errou nenhuma pergunta!"
    
    putStrLn "\n===================================================================="


exibirResumoPerguntas :: Pergunta -> IO ()
exibirResumoPerguntas p = do
    putStrLn ("ID: " ++ idQuestao p)
    putStrLn ("Pergunta: " ++ textoPergunta p)
    putStrLn ("Resposta correta: " ++ alternativa_Certa p)
    putStrLn "---"

iniciarQuiz :: [Pergunta] -> Nivel -> IO ()
iniciarQuiz perguntas nivelEscolhido = do
    resultado <- executarQuiz perguntas (ResultadoQuiz [] [] nivelEscolhido)
    exibirResumo resultado


executarQuiz :: [Pergunta] -> ResultadoQuiz -> IO ResultadoQuiz
executarQuiz [] resultado = return resultado
executarQuiz (p:ps) resultado = do
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
            exibirPergunta p
            
            putStr "Escolha uma alternativa: "
            alternativa_usuario <- getLine
            
            let comparacao = compara_acertou_errou alternativa_usuario (alternativa_Certa p)
            
            if comparacao
                then do
                    putStrLn "ACERTOU!"
                    salvarRespostaAcertada p
                    let novoResultado = resultado { acertos = p : acertos resultado }

                    executarQuiz ps novoResultado
                else do
                    putStrLn ("ERROU! A resposta correta era: " ++ alternativa_Certa p)
                    let novoResultado = resultado { erros = p : erros resultado }

                    executarQuiz ps novoResultado

main :: IO ()
main = do
  putStrLn "Bem vindo!"
  
  putStrLn "Escolha o nivel de dificuldade:"
  putStrLn "1 - Facil (pode errar ate 3 questoes)"
  putStrLn "2 - Medio (pode errar ate 2 questoes)" 
  putStrLn "3 - Dificil (pode errar ate 1 questao)"
  putStr "Sua escolha: "
  nivelInput <- getLine
  
  case stringParaNivel nivelInput of
    Nothing -> do
      putStrLn "Nivel invalido! Usando nivel Medio por padrao."
      main
    Just nivelEscolhido -> do
      putStrLn ("Nivel escolhido: " ++ show nivelEscolhido)
      
      putStrLn "Qual missao voce quer exibir?"
      missaoDesejada <- getLine
      
      putStrLn ("Quantas perguntas da missao '" ++ missaoDesejada ++ "' voce quer exibir?")
      numInput <- getLine
      
      let numPerguntas = read numInput :: Int
      
      todasAsPerguntas <- carregaPerguntas "quiz_completo.csv"
      
      let perguntasDaMissao = filter (\p -> missao p == missaoDesejada) todasAsPerguntas
      
      let perguntasParaExibir = take numPerguntas perguntasDaMissao
      
      if null perguntasParaExibir
        then putStrLn "Nenhuma pergunta encontrada para esta missao ou quantidade."
        else do
          putStrLn ("\n--- INICIANDO QUIZ COM " ++ show (length perguntasParaExibir) ++ " PERGUNTAS DA MISSAO '" ++ missaoDesejada ++ "' ---")
          putStrLn ("Nivel: " ++ show nivelEscolhido ++ " (maximo " ++ show (maxErrosPermitidos nivelEscolhido) ++ " erros)")
          putStrLn "Pressione Enter para comecar..."
          _ <- getLine
          iniciarQuiz perguntasParaExibir nivelEscolhido
