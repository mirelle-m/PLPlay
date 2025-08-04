module Auth where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import System.Directory (doesFileExist)
import Data.List (isInfixOf)
import Control.Monad (void)

mainAuth :: IO ()
mainAuth = UI.startGUI defaultConfig { jsStatic = Just "static" } setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Login PLPlay"
    let inputStyle = 
            [ ("padding", "8px")
            , ("margin-bottom", "10px")
            , ("width", "200px")
            , ("border-radius", "4px")
            , ("border", "1px solid #ccc")
            ]

    let buttonStyle = 
            [ ("background-color", "#d55a5d")
            , ("color", "white")
            , ("border", "none")
            , ("padding", "8px 16px")
            , ("cursor", "pointer")
            , ("border-radius", "7px")
            ]

    let pageStyle = 
            [ ("background-color", "#2b225d")
            , ("padding", "20px")
            , ("font-family", "'Press Start 2P', cursive")
            , ("color", "white")
            , ("display","flex")
            , ("justify-content","center")
            , ("align-items","center")
            , ("height","100vh")
        
            ]

    let messageStyle = 
            [ ("margin-top", "10px")
            , ("display", "block")
            , ("color", "white")
            , ("font-weight", "bold")
            ]


    titulo <- UI.h1 # set UI.style [("color", "white"),("font-family", "'Press Start 2P', cursive"),("font-size", "28px"), ("margin-bottom", "20px"),("border-radius", "4px")] #+ [string "Login PLPlay"]
    inputUser <- UI.input # set UI.class_ "myInput" # set UI.style inputStyle
    inputPass <- UI.input # set UI.type_ "password" # set UI.class_ "myInput" # set UI.style inputStyle

    -- Botão de login
    btnLogin <- UI.button # set UI.class_ "myButton" # set UI.style buttonStyle #+ [string "Entrar"]

    -- Mensagem
    lblMsg <- UI.span # set UI.style messageStyle

    -- Monta layout
    getBody window #+ 
        [ UI.column
            [ element titulo
            , UI.string "Username:"
            , element inputUser
            , UI.string "Senha:"
            , element inputPass
            , element btnLogin
            , element lblMsg
            ]
            # set UI.style 
                [ ("display", "flex")
                , ("flex-direction", "column")
                , ("gap", "10px")
                , ("align-items", "center")  -- <<< centraliza itens filhos, inclusive título
                , ("width", "250px")
                ]   
        ] # set UI.style pageStyle



    -- Ação de login
    on UI.click btnLogin $ \_ -> do
        username <- get value inputUser
        senha <- get value inputPass
        loginCheck username lblMsg

-- Verifica se o usuário existe no arquivo e cadastra se necessário
loginCheck :: String -> Element -> UI ()
loginCheck username lblMsg = do
    existe <- liftIO $ doesFileExist "user.txt"
    if existe
        then do
            conteudo <- liftIO $ readFile "user.txt"
            if ("username: " ++ username) `isInfixOf` conteudo
                then updateMessage lblMsg "✅ Autenticado com sucesso!"
                else do
                    liftIO $ appendFile "user.txt" ("username: " ++ username ++ "\nprogresso: Missao3 csharp\n")
                    updateMessage lblMsg "Usuário não encontrado. Cadastrado com sucesso!"
        else do
            liftIO $ writeFile "user.txt" ("username: " ++ username ++ "\nprogresso: Missao3 csharp\n")
            updateMessage lblMsg "Primeiro usuário cadastrado com sucesso!"

-- Atualiza mensagem de feedback
updateMessage :: Element -> String -> UI ()
updateMessage lblMsg msg = void $ element lblMsg # set UI.text msg
