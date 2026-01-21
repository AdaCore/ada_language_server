#!/bin/sh

# Fail at the first error
set -e

python compare.py \
 "$1/integration/vscode/ada/package.json" \
 "$1/integration/vscode/ada/deps-comments.json"
