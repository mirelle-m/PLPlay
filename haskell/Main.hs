-- Main.hs
module Main where

import Navegacao (escolherOpcao)

main :: IO ()
main = do
    let opcoes = ["🎮 Iniciar Jogo", "🗺️  Mapa", "🎒  Mochila","⚙️  Configurações","🚪 Sair"]
    putStrLn "Use as setas para navegar e Enter para selecionar."
    putStrLn "---------------------------------------------------"
    indiceEscolhido <- escolherOpcao opcoes
    putStrLn $ "Você escolheu a opção de índice: " ++ show indiceEscolhido
    putStrLn $ "Que é: " ++ opcoes !! indiceEscolhido
