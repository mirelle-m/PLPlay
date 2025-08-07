module Utils (centralizar, terminalWidth, terminalHeight) where

import qualified Terminal
import System.IO.Unsafe (unsafeDupablePerformIO)

terminalSize = unsafeDupablePerformIO Terminal.getTermSize
terminalHeight = fst terminalSize
terminalWidth = snd terminalSize

centralizar :: Int -> String -> String
centralizar largura texto =
    let espacos = replicate ((largura - length texto) `div` 2) ' '
    in espacos ++ texto
