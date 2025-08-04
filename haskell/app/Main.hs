module Main where
import Graphics.UI.Threepenny.Core
import Auth (mainAuth)
main :: IO ()
main = do
    mainAuth
