module Util where

import System.IO
import System.Console.ANSI
import System.Process(callCommand)

limpaTela :: IO()
limpaTela = callCommand "clear"
