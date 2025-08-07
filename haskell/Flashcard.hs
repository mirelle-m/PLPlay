module Flashcard where

import System.Random (randomRIO)
import System.IO (hFlush, stdout)
import Data.List.Split (splitOn)
import System.Console.ANSI (clearScreen)
import System.Random.Stateful (newStdGen)
import System.Random.Shuffle (shuffle')
import Util (limpaTela)

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
iniciaTreino _ = do
    limpaTela
    exibirBanner "../banners/inicio_treino.txt"
    _ <- getLine
    todos <- carregaFlashcards
    selecionados <- escolherAleatorios 10 todos
    mostrarTodos selecionados
    limpaTela
    exibirBanner "../banners/fim_treino.txt"
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> iniciaTreino []
        _   -> return ()

exibirBanner :: FilePath -> IO ()
exibirBanner caminho = do
    conteudo <- readFile caminho
    putStrLn conteudo
    return ()

mostrarTodos :: [Flashcard] -> IO ()
mostrarTodos [] = return ()
mostrarTodos (f:fs) = do
    limpaTela
    putStrLn $ pergunta f
    putStr "\n\n\nAperte Enter para ver a resposta"
    _ <- getLine
    putStrLn "\n\n\n=========================================================================\n\n\n"
    putStrLn $ resposta f
    putStr "\n\n\nAperte Enter para continuar"
    _ <- getLine
    limpaTela
    mostrarTodos fs