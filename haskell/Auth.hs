module Auth where

import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Char (isAlphaNum, isDigit, isLower, isUpper)
import Data.List (isInfixOf)
import Data.Typeable (Typeable)
import MapaMissoes (escolherMissao, imprimirMapa)
import Menu (menuPrincipal)
import Navegacao (escolherOpcao)
import System.Directory (doesFileExist)
import Utils (carregarLogo, centralizar, limparTela, larguraTerminal)

loopAutenticacao :: IO ()
loopAutenticacao = do
  result <- try autenticarUsuario :: IO (Either AutenticacaoException Bool)
  case result of
    Left (UsuarioInvalido msg) -> putStrLn ("Erro: " ++ msg) >> loopAutenticacao
    Left (SenhaInvalida msg) -> putStrLn ("Erro: " ++ msg) >> loopAutenticacao
    Right True -> menuPrincipal
    Right False -> putStrLn "Falha na autenticação."

data AutenticacaoException
  = UsuarioInvalido String
  | SenhaInvalida String
  deriving (Show, Typeable)

instance Exception AutenticacaoException

validarUsername :: String -> Either String ()
validarUsername nome
  | null nome = Left "Nome de usuário não pode ser vazio."
  | length nome < 3 = Left "Nome de usuário deve ter ao menos 3 caracteres."
  | length nome > 15 = Left "Nome de usuário deve ter no máximo 15 caracteres."
  | not (all (\c -> isAlphaNum c || c == '_') nome) = Left "Nome de usuário só pode conter letras, números e underscore (_)."
  | otherwise = Right ()


validarSenha :: String -> Either String ()
validarSenha senha
  | null senha = Left "Senha não pode ser vazia."
  | length senha < 6 = Left "Senha deve ter ao menos 6 caracteres."
  | not (any isUpper senha) = Left "Senha deve conter ao menos uma letra maiúscula."
  | not (any isLower senha) = Left "Senha deve conter ao menos uma letra minúscula."
  | not (any isDigit senha) = Left "Senha deve conter ao menos um número."
  | otherwise = Right ()


autenticarUsuario :: IO Bool
autenticarUsuario = do
  let largura = larguraTerminal
  putStrLn $ replicate largura '='
  putStrLn $ centralizar largura "🔐 AUTENTICAÇÃO"
  putStrLn $ replicate largura '='
  putStrLn "Digite seu username:"
  username <- getLine
  case validarUsername username of
    Left err -> throwIO (UsuarioInvalido err)
    Right () -> do
      putStrLn "Digite sua senha:"
      senha <- getLine
      case validarSenha senha of
        Left err -> throwIO (SenhaInvalida err)
        Right () -> do
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
