#!/usr/bin/env bash

if [ $(uname) = 'Darwin' ]; then
    ./scripts/install-macos-deps.sh
else
    echo "Unknown OS"
fi
