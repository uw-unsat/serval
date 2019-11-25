#!/usr/bin/env bash

if [ $(uname) = 'Darwin' ]; then
    ./scripts/install-macos-deps.sh
elif [[ $(uname) = 'Linux' ]]; then
    ./scripts/install-linux-deps.sh
else
    echo "Unknown OS"
fi
