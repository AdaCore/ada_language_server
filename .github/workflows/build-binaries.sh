#!/bin/bash
set -x -e
DEBUG=$1  # Value is '' or 'debug'
RUNNER_OS=$2  #  ${{ runner.os }} is Linux, Windiws, maxOS
TAG=$3 # For master it's 22.0.999, while for tag it's the tag itself
prefix=/tmp/ADALIB_DIR

if [ $RUNNER_OS = Windows ]; then
    prefix=/opt/ADALIB_DIR
    mount `cmd /c cd | cut -d\: -f1`:/opt /opt
fi

export GPR_PROJECT_PATH=$prefix/share/gpr:$PWD/subprojects/VSS/gnat:$PWD/subprojects/libadalang-tools/src:$PWD/subprojects/stubs
export CPATH=/usr/local/include:/mingw64/include
export LIBRARY_PATH=/usr/local/lib:/mingw64/lib
export DYLD_LIBRARY_PATH=/usr/local/lib
BRANCH=stable
mkdir -p $prefix
URL=https://bintray.com/reznikmm/libadalang/download_file\?file_path=libadalang-$RUNNER_OS-$BRANCH${DEBUG:+-dbg}-static.tar.gz
curl -L $URL | tar xzf - -C $prefix
gprinstall --uninstall gnatcoll || true
gprinstall --uninstall gpr || true

if [ "$DEBUG" = "debug" ]; then
    export BUILD_MODE=dev
else
    export BUILD_MODE=prod
fi

make LIBRARY_TYPE=static all check

function fix_rpath ()
{
    for R in `otool -l $1 |grep -A2 LC_RPATH |awk '/ path /{ print $2 }'`; do
        install_name_tool -delete_rpath $R $1
    done
    install_name_tool -change /usr/local/opt/gmp/lib/libgmp.10.dylib @rpath/libgmp.10.dylib $1
    install_name_tool -add_rpath @executable_path $1
}

if [ $RUNNER_OS = Windows ]; then
    tar czvf $RUNNER_OS${DEBUG:+-dbg}-$TAG.tar.gz -C integration/vscode/ada/ win32
elif [ $RUNNER_OS = macOS ]; then
    cp -v /usr/local/opt/gmp/lib/libgmp.10.dylib integration/vscode/ada/darwin/
    fix_rpath integration/vscode/ada/darwin/ada_language_server
    tar czvf $RUNNER_OS${DEBUG:+-dbg}-$TAG.tar.gz -C integration/vscode/ada/ darwin
else
    tar czvf $RUNNER_OS${DEBUG:+-dbg}-$TAG.tar.gz -C integration/vscode/ada/ linux
fi

if [ "$DEBUG" != "debug" ]; then
    strip -v integration/vscode/ada/*/ada_language_server*
fi
