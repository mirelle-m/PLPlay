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
    let caminho = "personagem.csv"
    let cabecalho = "nome,senha,progresso"
    let dadosUsuario = (nomeUsuario usuario) ++ "," ++ (senhaUsuario usuario) ++ "," ++ (progressoUsuario usuario)
    writeFile caminho (cabecalho ++ "\n" ++ dadosUsuario ++ "\n")

removeAspas :: String -> String
removeAspas str
  | length str >= 2 && head str == '"' && last str == '"' = init (tail str)
  | otherwise = str

carregaUsuario :: String -> IO (Maybe Usuario)
carregaUsuario userName = do
    existe <- doesFileExist "personagem.csv"
    if not existe
        then return Nothing
        else do
            conteudo <- readFile "personagem.csv"
            let linhas = drop 1 (lines conteudo)
                usuarios = map (map removeAspas . splitOn ',') linhas
                usuarioEncontrado = 
                    case filter (\cols -> not (null cols) && head cols == userName) usuarios of
                        (cols:_) | length cols >= 3 -> Just (Usuario (cols !! 0) (cols !! 1) (cols !! 2))
                        _ -> Nothing
            return usuarioEncontrado

-- atualizarProgresso :: String -> 