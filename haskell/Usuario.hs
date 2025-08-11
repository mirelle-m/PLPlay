module Usuario where

import Data.List
import System.Directory (doesFileExist)
import System.IO
import Text.Read (readMaybe)  


data Usuario = Usuario
  { nomeUsuario :: String
  , senhaUsuario :: String
  , progressoUsuario :: String
  } deriving (Show)

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c (h:t)
      | c == delimiter = []:h:t
      | otherwise = (c:h):t

salvarUsuario :: Usuario -> IO ()
salvarUsuario usuario = do
    let caminho = "../data/usuarios.csv"
    -- let cabecalho = "nome,senha,progresso"
    let dadosUsuario = (nomeUsuario usuario) ++ "," ++ (senhaUsuario usuario) ++ "," ++ (progressoUsuario usuario)
    appendFile caminho (dadosUsuario ++ "\n")

removeAspas :: String -> String
removeAspas str
  | length str >= 2 && head str == '"' && last str == '"' = init (tail str)
  | otherwise = str

carregaUsuario :: String -> IO (Maybe Usuario)
carregaUsuario userName = do
    existe <- doesFileExist "../data/usuarios.csv"
    if not existe
        then return Nothing
        else do
            conteudo <- readFile "../data/usuarios.csv"
            let linhas = drop 1 (lines conteudo)
                usuarios = map (map removeAspas . splitOn ',') linhas
                usuarioEncontrado = 
                    case filter (\cols -> not (null cols) && head cols == userName) usuarios of
                        (cols:_) | length cols >= 3 -> Just (Usuario (cols !! 0) (cols !! 1) (cols !! 2))
                        _ -> Nothing
            return usuarioEncontrado
  
atualizaLoginAtual :: String -> IO ()
atualizaLoginAtual nomeUsuario = do
    let caminho = "../data/loginAtual.txt"
    writeFile caminho (nomeUsuario ++ "\n")

recuperaLoginAtual :: IO String
recuperaLoginAtual = readFile "../data/loginAtual.txt"


addAspas :: String -> String
addAspas s = "\"" ++ s ++ "\""

atualizaProgresso :: String -> IO Bool
atualizaProgresso novoValor = do
    let caminho =  "../data/usuarios.csv"
    existe <- doesFileExist caminho
    if not existe
        then return False
        else do
            conteudo <- readFile caminho
            let linhas = lines conteudo
            if null linhas
                then return False
                else do
                    userName <- recuperaLoginAtual
                    let cabecalho = head linhas
                        dados = tail linhas
                        dadosAtualizados = map (atualizarSeNomeBater userName novoValor) dados
                        novoConteudo = unlines (cabecalho : dadosAtualizados)
                    writeFile caminho novoConteudo
                    return True

atualizarSeNomeBater :: String -> String -> String -> String
atualizarSeNomeBater nome novoValor linha =
    let colunas = map removeAspas (splitOn ',' linha)
    in if not (null colunas) && head colunas == nome && length colunas >= 3
        then intercalate "," [addAspas (colunas !! 0), addAspas (colunas !! 1), addAspas novoValor]
        else linha