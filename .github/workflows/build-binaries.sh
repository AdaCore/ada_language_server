#!/bin/bash
set -x -e
DEBUG=$1  # Value is '' or 'debug'
RUNNER_OS=$2  #  ${{ runner.os }} is Linux, Windiws, maxOS
TAG=$3 # For master it's 23.0.999, while for tag it's the tag itself
prefix=/tmp/ADALIB_DIR

if [ $RUNNER_OS = Windows ]; then
    prefix=/opt/ADALIB_DIR
    mount `cmd /c cd | cut -d\: -f1`:/opt /opt
fi

export GPR_PROJECT_PATH=$prefix/share/gpr:\
$PWD/subprojects/VSS/gnat:\
$PWD/subprojects/gnatdoc/gnat:\
$PWD/subprojects/libadalang-tools/src:\
$PWD/subprojects/spawn/gnat:\
$PWD/subprojects/stubs
export CPATH=/usr/local/include:/mingw64/include
export LIBRARY_PATH=/usr/local/lib:/mingw64/lib
export DYLD_LIBRARY_PATH=/usr/local/lib
export PATH=`ls -d $PWD/cached_gnat/*/bin |tr '\n' ':'`$PATH
echo PATH=$PATH

BRANCH=master

# Rebase PR on edge branch
if [[ ${GITHUB_REF##*/} != 2*.[0-9]*.[0-9]* ]]; then
    git config user.email "`git log -1 --pretty=format:'%ae'`"
    git config user.name  "`git log -1 --pretty=format:'%an'`"
    git config core.autocrlf
    git config core.autocrlf input
    git rebase --verbose origin/edge
fi

# Audit the npm packages
cd integration/vscode/ada
npm install
# Run npm audit to check for any vulnerabilities
npm audit
cd -

# Get libadalang binaries
mkdir -p $prefix
FILE=libadalang-$RUNNER_OS-$BRANCH${DEBUG:+-dbg}-static.tar.gz
aws s3 cp s3://adacore-gha-tray-eu-west-1/libadalang/$FILE . --sse=AES256
tar xzf $FILE -C $prefix
rm -f -v $FILE

which python
pip install --user e3-testsuite
python -c "import sys;print('e3' in sys.modules)"

if [ "$DEBUG" = "debug" ]; then
    export BUILD_MODE=dev
else
    export BUILD_MODE=prod
fi

pip install --user subprojects/langkit/

# Python used in GitHub CI on Windows can't understand
# make's notation of absolute path in form of /d/PATH,
# where /d is drive D: Let's use relative path instead
sed -i -e '/langkit/s/.{CURDIR}/../' subprojects/gpr/Makefile

make -C subprojects/gpr setup prefix=$prefix \
 GPR2KBDIR=./gprconfig_kb/db ENABLE_SHARED=no \
 ${DEBUG:+BUILD=debug} build-lib-static install-lib-static

make -C subprojects/templates-parser setup prefix=$prefix \
 ENABLE_SHARED=no \
 ${DEBUG:+BUILD=debug} build-static install-static

make LIBRARY_TYPE=static all check

function fix_rpath ()
{
    for R in `otool -l $1 |grep -A2 LC_RPATH |awk '/ path /{ print $2 }'`; do
        install_name_tool -delete_rpath $R $1
    done
    install_name_tool -change /usr/local/opt/gmp/lib/libgmp.10.dylib @rpath/libgmp.10.dylib $1
    install_name_tool -add_rpath @executable_path $1
}

if [ $RUNNER_OS = macOS ]; then
    cp -v /usr/local/opt/gmp/lib/libgmp.10.dylib integration/vscode/ada/darwin/
    fix_rpath integration/vscode/ada/darwin/ada_language_server
fi

if [ "$DEBUG" != "debug" ]; then
    strip -v integration/vscode/ada/*/ada_language_server*
fi
