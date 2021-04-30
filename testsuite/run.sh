#!/usr/bin/env sh

if [ "$OS" = "Windows_NT" ]; then
    PYTHON=python.exe
else
    PYTHON=python3
fi

# This is a convenience command-line driver made for development mode
$PYTHON ./run-tests --failure-exit-code 1 --loglevel INFO --show-error-output $@
