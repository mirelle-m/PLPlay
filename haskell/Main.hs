module Main where
import Graphics.UI.Threepenny.Core
import Auth (mainAuth)
import Inicial (paginaInicial)
main :: IO ()
main = do
    paginaInicial
    mainAuth
