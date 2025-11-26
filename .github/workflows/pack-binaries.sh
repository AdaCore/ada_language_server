#!/bin/bash
set -x -e
TAG=$1 # For master it's 24.0.999, while for tag it's the tag itself

NODE_PLATFORM=$(node -e "console.log(process.platform)")
NODE_ARCH=$(node -e "console.log(process.arch)")
ext_dir=integration/vscode/ada

function create_changelog() {
   # Replace the \<next> section with the tag
   sed -e "s/^## \\\\<next>$/## $TAG/" <CHANGELOG.md
}

create_changelog >"$ext_dir/CHANGELOG.md"

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

   # Determine if this is a pre-release. The convention is based on the last
   # digit of the version:
   #  0 --> pre-release
   #  1 -->     release
   #  2 --> pre-release (based on master branch in edge mode)
   #  3 -->     release (based on master branch in edge mode)
   case "$TAG" in
   *1 | *3)
      pre_release=""
      ;;
   *)
      # default to a pre-release
      pre_release=--pre-release
      ;;
   esac

   # Create the VSIX
   npx vsce package --target "${NODE_PLATFORM}-${NODE_ARCH}" $pre_release
)

# Move all .vsix packages to the root of the checkout
mv -v "$ext_dir"/*.vsix .
