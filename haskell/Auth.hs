module Auth (autenticar) where

import System.IO
import System.Directory (doesFileExist)
import Data.List (isInfixOf)
import Utils (centralizar, terminalWidth)

-- Retorna True se autenticado ou cadastrado com sucesso
autenticar :: IO Bool
autenticar = do
    let largura = terminalWidth
    putStrLn $ replicate largura '='
    putStrLn $ centralizar largura "🔐 AUTENTICAÇÃO"
    putStrLn $ replicate largura '='
    putStrLn "Digite seu username:"
    username <- getLine
    putStrLn "Digite sua senha:"
    _ <- getLine  -- senha não é usada, mas mantida para o fluxo
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