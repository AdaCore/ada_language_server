#!/bin/bash

set -e -x

INSTALL_DIR=$PWD/../gnat
export PATH=$INSTALL_DIR/bin:$PATH

function download_gnat()
{
    if [ ! -f $GNAT_INSTALLER ]; then
        mkdir -p `dirname $GNAT_INSTALLER`
        # Use --progress=dot:giga to ensure travis doesn't give up for lack of progress
        wget --progress=dot:giga -O $GNAT_INSTALLER $1
    fi

    git clone https://github.com/AdaCore/gnat_community_install_script.git
    sh gnat_community_install_script/install_package.sh $GNAT_INSTALLER $INSTALL_DIR
    $INSTALL_DIR/bin/gprinstall --uninstall gnatcoll
}

function linux_before_install()
{
    echo INSTALL_DIR=$INSTALL_DIR
    GNAT_INSTALLER=$HOME/cache/gnat-install

    download_gnat https://community.download.adacore.com/v1/0cd3e2a668332613b522d9612ffa27ef3eb0815b\
?filename=gnat-community-2019-20190517-x86_64-linux-bin

    wget -nv -O- https://dl.bintray.com/reznikmm/libadalang/libadalang-stable-linux.tar.gz \
        | tar xzf - -C $INSTALL_DIR
    sudo apt-get update
    sudo apt-get -y install chrpath
}

function linux_script()
{
    TAG=${TRAVIS_TAG:-latest}
    sed -i -e "s/VERSION/$TAG/g" integration/travis/bintray.json
    make LIBRARY_TYPE=relocatable check

    # Make the VS Code plugin
    make vscode

    # Test the VS Code plugin
    /usr/bin/Xvfb :101 -screen 0 1024x768x24 > /dev/null 2>&1 &
    DISPLAY=:101 make vscode-test

    integration/travis/deploy.sh linux
}


function osx_before_install()
{
    echo INSTALL_DIR=$INSTALL_DIR
    GNAT_INSTALLER=$HOME/cache/gnat-community-2019-20190517-x86_64-darwin-bin.dmg

    download_gnat https://community.download.adacore.com/v1/5a7801fc686e86de838cfaf7071170152d81254d\
?filename=gnat-community-2019-20190517-x86_64-darwin-bin.dmg

    wget -nv -O- https://dl.bintray.com/reznikmm/libadalang/libadalang-stable-osx.tar.gz \
        | tar xzf - -C $INSTALL_DIR
}

function drop_rpath ()
{
    for R in `otool -l $1 |grep -A2 LC_RPATH |awk '/ path /{ print $2 }'`; do
        install_name_tool -delete_rpath $R $1
    done
}

function osx_copy_dylibs()
{
    DIR=$1
    LIB=../gnat/lib

    for J in \
        $LIB/libadalang.relocatable/libadalang \
        $LIB/gnatcoll_gmp.relocatable/libgnatcoll_gmp \
        $LIB/langkit_support.relocatable/liblangkit_support \
        $LIB/gnatcoll_iconv.relocatable/libgnatcoll_iconv \
        $LIB/gnatcoll.relocatable/libgnatcoll \
        $LIB/gpr/relocatable/gpr/libgpr \
        $LIB/xmlada/xmlada_schema.relocatable/libxmlada_schema \
        $LIB/xmlada/xmlada_dom.relocatable/libxmlada_dom \
        $LIB/xmlada/xmlada_sax.relocatable/libxmlada_sax \
        $LIB/xmlada/xmlada_input.relocatable/libxmlada_input_sources \
        $LIB/xmlada/xmlada_unicode.relocatable/libxmlada_unicode \
        $LIB/gcc/x86_64-apple-darwin*/8.3.1/adalib/libgnat-2019 \
        $LIB/gcc/x86_64-apple-darwin*/8.3.1/adalib/libgnarl-2019 \
        /usr/local/opt/gmp/lib/libgmp.10
    do
        cp -v $J.dylib $DIR
        drop_rpath $DIR/`basename $J.dylib`
    done

    for J in $DIR/libgnatcoll_gmp.dylib $DIR/ada_language_server; do
        install_name_tool -change /usr/local/opt/gmp/lib/libgmp.10.dylib @rpath/libgmp.10.dylib $J
    done

    drop_rpath $DIR/ada_language_server
    install_name_tool -add_rpath @executable_path $DIR/ada_language_server
}

function osx_script()
{
    export LIBRARY_PATH=/usr/local/lib        # To find GMP
    make OS=osx LIBRARY_TYPE=relocatable all
    osx_copy_dylibs .obj/server/
    .obj/server/ada_language_server < /dev/null
    make OS=osx LIBRARY_TYPE=relocatable check
    integration/travis/deploy.sh darwin
}

${TRAVIS_OS_NAME}_$1
