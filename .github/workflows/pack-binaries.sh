#!/bin/bash
set -x -e
DEBUG=$1 # Value is '' or 'debug'
VSCE_TOKEN=$2
OVSX_TOKEN=$3
TAG=$4 # For master it's 24.0.999, while for tag it's the tag itself

function make_change_log() {
   echo "# Release notes"
   echo ""
   for TAG_ID in $(git tag --list --sort=-v:refname '2*'); do
      DATE=$(git show --no-patch --format=Date:%ad --date=short "$TAG_ID" |
         grep Date: | sed -e s/Date://)
      echo "## $TAG_ID ($DATE)"
      git show --no-patch --format=%n "$TAG_ID" | sed -e '1,/Release notes/d'
   done
}

function os_to_node_platform() {
   case "$1" in
   Linux)
      echo -n "linux"
      ;;
   Windows)
      echo -n "win32"
      ;;
   macOS)
      echo -n "darwin"
      ;;
   esac
}

function cross_to_node_arch() {
   case "$1" in
   aarch64)
      echo -n "arm64"
      ;;
   *)
      echo -n "x64"
      ;;
   esac
}

ext_dir=integration/vscode/ada

(
   cd "$ext_dir"

   # Set package version based on the Git tag
   sed -i -e "/version/s/[0-9][0-9.]*/$TAG/" package.json
   # Change extension ID and name if we're in debug mode
   [ -z "$DEBUG" ] || sed -i -e '/^    "name"/s/ada/ada-debug/' \
      -e '/displayName/s/Ada & SPARK/Ada & SPARK (with debug info)/' package.json

   # Install NPM deps
   npm -v
   node -v
   which node
   npm install

   # Create change log
   make_change_log >CHANGELOG.md
)

# At the moment we only create VSIX-es for macOS on GitHub. Other platforms are
# provided elsewhere.
# shellcheck disable=SC2043
for OS in macOS Windows Linux; do
   for CROSS in "" "aarch64"; do
      source=als-"$OS"-"$DEBUG""$CROSS"
      if [ -d "$source" ]; then
         # Make sure the file are executable
         chmod -R -v +x "$source"
         # Copy the binary in place
         rsync -rva als-$OS-"$DEBUG""$CROSS"/ "$ext_dir"/
         # Delete debug info
         rm -rf -v "$ext_dir"/{arm,arm64,x64}/{linux,darwin,win32}/*.{debug,dSYM}

         (
            cd "$ext_dir"
            # Create the VSIX
            npx vsce package --target "$(os_to_node_platform $OS)-$(cross_to_node_arch $CROSS)"
         )

         # Cleanup the binary directory
         rm -rf -v "$ext_dir"/{arm,arm64,x64}
      fi
   done
done

# Move all .vsix packages to the root of the checkout
mv -v "$ext_dir"/*.vsix .
# Discard the package.json and package-lock.json changes
git checkout "$ext_dir"/package*.json
