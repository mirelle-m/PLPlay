module MapaMissoes where

import qualified Terminal
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.List (isPrefixOf)
import Utils(centralizar, larguraTerminal, mostrarLogoCentralizada)
import Navegacao (escolherOpcaoComTitulo)
import Inicial (mostrarLogoAnimada)
 
missoesMapeadas :: [(String, [String])]
missoesMapeadas =
  [ ("Primeiro Estágio"
    , [ "Missão 1: Introdução - Históricos e Características"
      , "Missão 2: Classificação e Características"
      , "Missão 3: Valores, Tipos e Sistema de Tipos"
      , "Missão 4: Paradigma Imperativo"
      , "Chefão 1: Batalha dos Fundamentos"
      ])
  , ("Segundo Estágio"
    , [ "Missão 5: Paradigma Funcional"
      , "Chefão 2: Guardião da Recursão"
      ])
  , ("Terceiro Estágio"
    , [ "Missão 6: Paradigma Lógico"
      , "Chefão 3: Mestre da Dedução"
      ])
  ]

missoesMapeadasNomes :: [String]
missoesMapeadasNomes = ["🧭 Missão 1: Introdução - Históricos e Características"
      ,"🧭 Missão 2: Classificação e Características"
      ,"🧭 Missão 3: Valores, Tipos e Sistema de Tipos"
      ,"🧭 Missão 4: Paradigma Imperativo"
      ,"🧭 Missão 5: Paradigma Funcional"
      ,"🧭 Missão 6: Paradigma Lógico"]

chefoesMapeadas:: [String]
chefoesMapeadas = ["👾 Chefão 1: Batalha dos Fundamentos"
      ,"👾 Chefão 2: Guardião da Recursão"
      ,"👾 Chefão 3: Mestre da Dedução"]

gerarLinhasMapa :: [(String, [String])] -> [String]
gerarLinhasMapa = concatMap gerarLinhasEstagio
  where
    gerarLinhasEstagio (estagio, missoes) =
      let titulo = "📍 " ++ estagio
          linhasMissoes = map (\m -> "   " ++ emoji m ++ m) missoes
      in titulo : linhasMissoes

    emoji m = if "Chefão" `elem` words m then "👾 " else "🧭 "


imprimirMapa :: IO ()
imprimirMapa = do
  mostrarLogoCentralizada "../banners/mapa.txt"


escolherMissao :: IO String
escolherMissao = do
  let nomesEstagios = map fst missoesMapeadas
  let estagios = map ("📍 " ++) nomesEstagios
  idxEstagio <- escolherOpcaoComTitulo "../banners/escolha_estagio.txt" estagios
  let (nomeEstagio, missoes) = missoesMapeadas !! idxEstagio
  escolherMissaoDoEstagio nomeEstagio missoes


escolherMissaoDoEstagio :: String -> [String] -> IO String
escolherMissaoDoEstagio nomeEstagio missoes = do
  let nomeEstagioPath = case nomeEstagio of
        "Primeiro Estágio" -> "../banners/primeiro_estagio.txt"
        "Segundo Estágio"  -> "../banners/segundo_estagio.txt"
        "Terceiro Estágio" -> "../banners/terceiro_estagio.txt"
        _                  -> "../banners/default.txt"
  let largura = larguraTerminal
  idxMissao <- escolherOpcaoComTitulo nomeEstagioPath (map formatarMissao missoes)
  return (missoes !! idxMissao)


formatarMissao :: String -> String
formatarMissao m =
  let emoji = if "Chefão" `elem` words m then "👾 " else "🧭 "
  in emoji ++ m


escolherMissaoComRestricao :: String -> IO String
escolherMissaoComRestricao maxDesbloqueada = do
    let maxNum = read maxDesbloqueada :: Int
    let nomesEstagios = map fst missoesMapeadas
    let estagios = map ("📍 " ++) nomesEstagios

    idxEstagio <- escolherOpcaoComTitulo "../banners/escolha_estagio.txt" estagios
    let (nomeEstagio, missoes) = missoesMapeadas !! idxEstagio

    let missoesLiberadas =
            take maxNum missoes -- só mostra até a missão desbloqueada

    idxMissao <- escolherOpcaoComTitulo (bannerDoEstagio nomeEstagio) (map formatarMissao missoesLiberadas)
    return (extrairNumero (missoesLiberadas !! idxMissao))

bannerDoEstagio :: String -> FilePath
bannerDoEstagio "Primeiro Estágio" = "../banners/primeiro_estagio.txt"
bannerDoEstagio "Segundo Estágio"  = "../banners/segundo_estagio.txt"
bannerDoEstagio "Terceiro Estágio" = "../banners/terceiro_estagio.txt"
bannerDoEstagio _                  = "../banners/default.txt"

extrairNumero :: String -> String
extrairNumero m =
    case words m of
        ("Missão":n:_) -> filter (`elem` ['0'..'9']) n
        ("Chefão":n:_) -> filter (`elem` ['0'..'9']) n
        _              -> ""