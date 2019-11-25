#!/usr/bin/env bash

brew tap riscv/riscv
brew install wget gcc llvm riscv-tools git

case $RACKET_VERSION in
    7.5-cs )
        RACKET_URL="https://mirror.racket-lang.org/installers/7.5/racket-7.5-x86_64-macosx-cs.dmg"
        ;;
esac

wget "$RACKET_URL"
hdiutil attach $(basename "$RACKET_URL")
cp -r "/Volumes/Racket*/Racket*" "/Applications"

for name in $(ls /Applications/Racket*/bin); do
    ln -sf "/Applications/Racket*/bin/${name}" "/usr/local/bin/${name}"
done

raco pkg install --auto -i rosette

git clone 'https://github.com/emina/rosette.git'
raco pkg install --auto -i --no-setup ./rosette && raco setup -Dl rosette
