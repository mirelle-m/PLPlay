module Main where

import Inicial (paginaInicial)
import Auth (loopAutenticacao)
main :: IO ()
main = do
    paginaInicial
    loopAutenticacao