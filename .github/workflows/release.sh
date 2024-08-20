#!/bin/bash
set -x -e
GITHUB_ACCESS_TOKEN=$1
TAG=$2                # Release name/tag
NODE_ARCH_PLATFORM=$3 # One of:
# arm64/darwin
# arm64/linux
# x64/darwin
# x64/linux
# x64/win32

if [[ -z "$NODE_ARCH_PLATFORM" ]]; then
   # Get architecture and platform information from node.
   NODE_PLATFORM=$(node -e "console.log(process.platform)")
   NODE_ARCH=$(node -e "console.log(process.arch)")
   NODE_ARCH_PLATFORM=$NODE_ARCH/$NODE_PLATFORM
   NAME=$NODE_ARCH-$NODE_PLATFORM
else
   NAME=${NODE_ARCH_PLATFORM%/*}-${NODE_ARCH_PLATFORM#*/}
fi

# For tags `actions/checkout@v2` action fetches a tag's commit, but
# not the tag annotation itself. Let's refetch the tag from origin.
# This makes `git show --no-patch --format=%n $TAG` work again.
git tag --delete "$TAG"
git fetch --tags

function release_notes() {
   echo "# Release notes"

   # Select the content of the first section of CHANGELOG.md
   sed -n -e '/^## \\<next>/,/^##/p' <CHANGELOG.md | tail -n +2 | head -n -1

   COMMITS=commits.txt

   if [ -f "$COMMITS" ]; then
      {
         echo "# Dependency commits"
         echo ""
         cat "$COMMITS"
         echo ""
      }
   fi
}

release_notes >release_notes.md

# Try to create a release
jq --null-input --arg tag "$TAG" \
   --rawfile body release_notes.md \
   '{"tag_name": $tag, "name": $tag, "body": $body}' |
   curl -v \
      -X POST \
      -H "Accept: application/vnd.github+json" \
      -H "Authorization: token $GITHUB_ACCESS_TOKEN" \
      -H 'Content-Type: application/json' \
      "https://api.github.com/repos/$GITHUB_REPOSITORY/releases" \
      -d "@-"

rm -f release_notes.md

# Get asset upload url, drop "quotes" around it and {parameters} at the end
upload_url=$(curl \
   -H "Accept: application/vnd.github+json" \
   -H "Authorization: token $GITHUB_ACCESS_TOKEN" \
   "https://api.github.com/repos/$GITHUB_REPOSITORY/releases/tags/$TAG" |
   jq -r '.upload_url | rtrimstr("{?name,label}")')

echo "upload_url=$upload_url"

FILE=$NAME.tar.gz
tar czvf "$FILE" "integration/vscode/ada/$NODE_ARCH_PLATFORM" commits.txt

# Upload $FILE as an asset to the release
curl \
   -X POST \
   -H "Accept: application/vnd.github+json" \
   -H "Authorization: token $GITHUB_ACCESS_TOKEN" \
   -H 'Content-Type: application/tar+gzip' \
   --data-binary "@$FILE" \
   "$upload_url?name=$FILE"
