#!/usr/bin/env bash

brew tap riscv/riscv
brew install llvm riscv-tools

case $RACKET_VM in
    cs )
        brew cask install racket-cs
        ;;
    racket )
        brew cask install racket
        ;;
esac

git clone 'https://github.com/emina/rosette.git'
raco pkg install --auto -i --no-setup ./rosette && raco setup -Dl rosette
