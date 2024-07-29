#!/bin/bash
set -x -e
TAG=$1 # For master it's 24.0.999, while for tag it's the tag itself

NODE_PLATFORM=$(node -e "console.log(process.platform)")
NODE_ARCH=$(node -e "console.log(process.arch)")
ext_dir=integration/vscode/ada

(
   cd "$ext_dir"

   # Set package version based on the Git tag
   sed -i -e "/version/s/[0-9][0-9.]*/$TAG/" package.json

   # Install NPM deps
   npm -v
   node -v
   which node
   npm install

   # Delete debug info
   rm -rf -v {arm,arm64,x64}/{linux,darwin,win32}/*.{debug,dSYM}
   # Create the VSIX
   npx vsce package --target "${NODE_PLATFORM}-${NODE_ARCH}"
)

# Move all .vsix packages to the root of the checkout
mv -v "$ext_dir"/*.vsix .
