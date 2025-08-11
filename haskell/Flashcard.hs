module Flashcard where

import System.IO (hFlush, stdout)
import Data.List.Split (splitOn)
import System.Random.Stateful (newStdGen)
import System.Random.Shuffle (shuffle')
import Utils (limparTela, larguraTerminal, centralizar)

data Flashcard = Flashcard {
    pergunta :: String,
    resposta :: String
} deriving (Show)


carregarFlashcards :: IO [Flashcard]
carregarFlashcards = do
    conteudo <- readFile "../data/flashcards.csv"
    let linhas = tail . lines $ conteudo
    let flashcards = [ Flashcard p r | linha <- linhas, let conjuntos = splitOn ";" linha, [p, r] <- [conjuntos] ]
    return flashcards


escolherPerguntasAleatorias :: Int -> [a] -> IO [a]
escolherPerguntasAleatorias n lista = do
    let len = length lista
    if n >= len 
        then return lista 
        else do 
            indices <- gerarIndices len
            let indicesAleatorios = take n indices
            return [lista !! i | i <- indicesAleatorios]


gerarIndices :: Int -> IO [Int]
gerarIndices max = do
    g <- newStdGen
    let indices = [0 .. max - 1]
    return (shuffle' indices max g)


iniciarTreino :: [Flashcard] -> IO ()
iniciarTreino _ = do
    limparTela
    exibirBanner "../banners/inicio_treino.txt"
    _ <- getLine
    todos <- carregarFlashcards
    selecionados <- escolherPerguntasAleatorias 10 todos
    mostrarFlashcards selecionados
    limparTela
    exibirBanner "../banners/fim_treino.txt"
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> iniciarTreino []
        _   -> return ()


exibirBanner :: FilePath -> IO ()
exibirBanner caminho = do
    conteudo <- readFile caminho
    putStrLn conteudo
    return ()


mostrarFlashcards :: [Flashcard] -> IO ()
mostrarFlashcards [] = return ()
mostrarFlashcards (f:fs) = do
    limparTela
    let largura = larguraTerminal
    putStrLn $ replicate largura '='
    putStrLn $ "\n\n" ++ centralizar largura (pergunta f) ++ "\n\n"
    putStrLn $ replicate largura '='
    putStr "\n\nAperte Enter para ver a resposta\n\n\n"
    _ <- getLine
    putStrLn $ replicate largura '='
    putStrLn $ "\n\n" ++ centralizar largura (resposta f) ++ "\n\n"
    putStrLn $ replicate largura '='
    putStr "\n\nAperte Enter para continuar"
    _ <- getLine
    limparTela
    mostrarFlashcards fs