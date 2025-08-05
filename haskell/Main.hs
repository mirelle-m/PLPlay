-- Main.hs
module Main where
import Auth (mainAuth)
import Inicial (paginaInicial)

import Navegacao (escolherOpcao)


main :: IO ()
main = do
    paginaInicial
    mainAuth
