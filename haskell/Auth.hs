module Auth where

import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Char (isAlphaNum, isDigit, isLower, isUpper)
import Data.List (isInfixOf)
import Data.Typeable (Typeable)
import MapaMissoes (escolherMissao, imprimirMapa)
import Menu (menuPrincipal)
import Navegacao (escolherOpcaoComTitulo)
import System.Directory (doesFileExist)
import Utils (carregarLogo, centralizar, limparTela, larguraTerminal)
import Usuario

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
          let usuarioNovo = Usuario username senha "1"
          exists <- doesFileExist "personagem.csv"
          if exists
            then do
              usuario <- carregaUsuario username
              case usuario of
                Nothing -> do
                  salvarUsuario usuarioNovo
                  putStrLn "✅ Cadastro realizado com sucesso!"
                  return True
                Just u -> do
                  if senha == (senhaUsuario u) then do
                    putStrLn "✅ Autenticado com sucesso!"
                    return True
                  else do 
                    putStrLn "❌ Senha incorreta! Tente Novamente!"
                    return True
          else do
              putStrLn "Ops! Algo deu errado! Tente novamente!"
              return True
