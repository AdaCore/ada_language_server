#!/bin/bash
set -x -e
DEBUG=$1 # Value is '' or 'debug'
VSCE_TOKEN=$2
OVSX_TOKEN=$3
TAG=$4 # For master it's 24.0.999, while for tag it's the tag itself

function make_change_log()
{
    echo "# Release notes"
    echo ""
    for TAG_ID in `git tag --list --sort=-v:refname '2*'` ; do
        DATE=`git show --no-patch --format=Date:%ad --date=short $TAG_ID |\
          grep Date: | sed -e s/Date://`
        echo "## $TAG_ID ($DATE)"
        git show --no-patch --format=%n $TAG_ID | sed -e '1,/Release notes/d'
    done
}

chmod -R -v +x als-*-$DEBUG als-Linux-${DEBUG}aarch64

for X in Linux macOS Windows ; do
    rsync -rva als-$X-$DEBUG/ integration/vscode/ada/
done

rsync -rva als-Linux-${DEBUG}aarch64/ integration/vscode/ada/

# VS Code is supported on arm, arm64 and x64 so we only consider those
# architectures
rm -rf -v integration/vscode/ada/{arm,arm64,x64}/{linux,darwin,win32}/*.{debug,dSYM}

pushd integration/vscode/ada
sed -i -e "/version/s/[0-9][0-9.]*/$TAG/" package.json
[ -z "$DEBUG" ] || sed -i -e '/^    "name"/s/ada/ada-debug/' \
                          -e '/displayName/s/Ada/Ada (with debug info)/' package.json

npm -v; node -v; which node
npm install
sudo npm install -g @vscode/vsce --unsafe-perm
sudo npm install -g esbuild --unsafe-perm
make_change_log > CHANGELOG.md
if [[ ${GITHUB_REF##*/} = 2*.[0-9]*.[0-9]* ]] ; then
    vsce publish -p "$VSCE_TOKEN" || true
    npx ovsx publish -p "$OVSX_TOKEN" || true
fi
vsce package || true
popd
mv -v integration/vscode/ada/*.vsix .
git checkout integration/vscode/ada/package.json
rm -rf integration/vscode/ada/{arm,arm64,x64}/{linux,darwin,win32}
