#!/bin/bash
set -x -e
DEBUG=$1 # Value is '' or 'debug'
VSCE_TOKEN=$2
OVSX_TOKEN=$3
TAG=$4 # For master it's 23.0.999, while for tag it's the tag itself

function make_change_log()
{
    echo "# Release notes"
    echo ""
    for TAG_ID in `git tag --list '2*' | tac` ; do
        DATE=`git show --no-patch --format=Date:%ad --date=short $TAG_ID |\
          grep Date: | sed -e s/Date://`
        echo "## $TAG_ID ($DATE)"
        git show --no-patch --format=%n $TAG_ID | sed -e '1,/Release notes/d'
    done
}

chmod -R -v +x als-*-$DEBUG
for X in Linux macOS Windows ; do mv -v -f als-$X-$DEBUG/* integration/vscode/ada/; done
pushd integration/vscode/ada
sed -i -e "/version/s/[0-9][0-9.]*/$TAG/" package.json
[ -z "$DEBUG" ] || sed -i -e '/^    "name"/s/ada/ada-debug/' \
                          -e '/displayName/s/Ada/Ada (with debug info)/' package.json
npm install
sudo npm install -g vsce --unsafe-perm
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
rm -rf integration/vscode/ada/{linux,darwin,win32}
