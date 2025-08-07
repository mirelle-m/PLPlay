#!/bin/bash

echo "Atualizando o sistema..."
sudo apt update

echo "Instalando o cabal-install..."
sudo apt install -y cabal-install

echo "Atualizando cabal..."
cabal update

echo "Instalando as bibliotecas necessárias..."
cabal install split --lib
cabal install directory --lib
cabal install process --lib
cabal install random --lib
cabal install ansi-terminal
cabal install ansi-terminal --lib
cabal install random-shuffle --lib

echo "Todas as dependências foram instaladas com sucesso! Agora está tudo pronto para você estudar com o PLPlay!"

echo "Executando o programa..."
cd haskell
runhaskell Main.hs