module Flashcard where

import System.Random (randomRIO)
import System.IO (hFlush, stdout)
import Data.List.Split (splitOn)
import System.Console.ANSI (clearScreen)
import System.Random.Stateful (newStdGen)
import System.Random.Shuffle (shuffle')

data Flashcard = Flashcard {
    pergunta :: String,
    resposta :: String
} deriving (Show)


carregaFlashcards :: IO [Flashcard]
carregaFlashcards = do
    conteudo <- readFile "../data/flashcards.csv"
    let linhas = tail . lines $ conteudo
    let flashcards = [ Flashcard p r | linha <- linhas, let conjuntos = splitOn ";" linha, [p, r] <- [conjuntos] ]
    return flashcards


escolherAleatorios :: Int -> [a] -> IO [a]
escolherAleatorios n lista = do
    let len = length lista
    if n >= len then return lista
    else do
        indices <- gerarIndices len
        let indicesAleatorios = take n indices
        return [lista !! i | i <- indicesAleatorios]


gerarIndices :: Int -> IO [Int]
gerarIndices max = do
    g <- newStdGen
    let indices = [0 .. max - 1]
    return (shuffle' indices max g)


iniciaTreino :: [Flashcard] -> IO ()
iniciaTreino flashcards = do
    clearScreen
    putStrLn "Iniciando treino com flashcards...\n"
    mostrarTodos flashcards
    putStrLn "\nTreino finalizado!\n"
    putStrLn "1 - Repetir treino"
    putStrLn "2 - Voltar ao menu"
    putStr "Escolha: "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> iniciaTreino flashcards
        _   -> return ()


mostrarTodos :: [Flashcard] -> IO ()
mostrarTodos [] = return ()
mostrarTodos (f:fs) = do
    clearScreen
    putStrLn $ "Pergunta:\n\n" ++ pergunta f
    putStr "\n(Aperte Enter para ver a resposta)"
    _ <- getLine
    putStrLn $ "\nResposta:\n\n" ++ resposta f
    putStr "\n(Aperte Enter para continuar)"
    _ <- getLine
    mostrarTodos fs