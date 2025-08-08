#!/bin/bash

echo "Atualizando o sistema..."
sudo apt update

echo "Instalando o GHC..."
sudo apt install -y ghc

echo "Instalando as bibliotecas necessárias via cabal..."
cabal update
cabal install split --lib
cabal install directory --lib
cabal install process --lib
cabal install random --lib
cabal install time --lib
cabal install async --lib
cabal install ansi-terminal --lib

echo "Executando o programa..."

cd haskell || { echo "Diretório haskell não encontrado! Abortando."; exit 1; }
runghc Main.hs
