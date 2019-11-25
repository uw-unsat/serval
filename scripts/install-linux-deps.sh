#!/usr/bin/env bash

apt-get update
apt-get install -y llvm gcc build-essential

case $RACKET_VM in
    cs )
        URL='https://mirror.racket-lang.org/installers/7.5/racket-7.5-x86_64-linux-cs.sh'
        ;;
    racket )
        URL='https://mirror.racket-lang.org/installers/7.5/racket-7.5-x86_64-linux.sh'
        ;;
esac

wget "$URL"
FILE=$(basename "$URL")
chmod +x "$FILE"
echo 'yes\n1' | "./${FILE}"
rm -fv "${FILE}"

raco pkg install --auto -i --no-setup rosette && raco setup -Dl rosette
