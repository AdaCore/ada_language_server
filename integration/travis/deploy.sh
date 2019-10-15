#!/bin/bash -e

set -x

PLATFORM=$1
DIR=integration/vscode/ada/$PLATFORM
TAG=${TRAVIS_TAG:-latest}

function linux_deploy()
{
    LIB=/opt/gnat/lib

    cp --dereference $LIB/lib{adalang,langkit_support}.so $DIR
    cp --dereference $LIB/libgnatcoll{_gmp,_iconv,}.so.0 $DIR
    cp --dereference $LIB/libxmlada_{dom,input_sources,sax,schema,unicode}.so.2019 $DIR
    cp --dereference $LIB/libgpr.so $DIR

    chrpath -r '$ORIGIN/' $DIR/*

    cp $LIB/gcc/x86_64-pc-linux-gnu/8.3.1/adalib/lib{gnat,gnarl}-2019.so $DIR

    tar czvf /upload/$PLATFORM-$TAG-dbg.tar.gz -C integration/vscode/ada/ $PLATFORM

    strip $DIR/*
    tar czvf /upload/$PLATFORM-$TAG.tar.gz -C integration/vscode/ada/ $PLATFORM
}

function drop_rpath ()
{
    for R in `otool -l $1 |grep -A2 LC_RPATH |awk '/ path /{ print $2 }'`; do
        install_name_tool -delete_rpath $R $1
    done
}


function darwin_deploy()
{
    LIB=../gnat/lib
    ls -l $LIB/gcc

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

    mkdir upload
    tar czvf upload/$PLATFORM-$TAG.tar.gz -C integration/vscode/ada/ $PLATFORM
}

function vsix_deploy()
{
    sed -e 's/:white_check_mark:/Yes               /g' README.md > \
      integration/vscode/ada/README.md
    cp -f LICENSE integration/vscode/ada/

    [ -z "$TRAVIS_TAG" ] || sed -i -e "/version/s/[0-9][0-9.]*/$TAG/" \
      integration/vscode/ada/package.json

    pushd integration/vscode/ada

    wget -nv -Owin32.zip \
         "https://dl.bintray.com/reznikmm/ada-language-server/win32-$TAG.zip"
    unzip win32.zip
    rm win32.zip

    for J in darwin linux ; do
        wget -nv -O- \
             "https://dl.bintray.com/reznikmm/ada-language-server/$J-$TAG.tar.gz" |\
             tar xzvf -
    done

    npm install
    npm install -g vsce
    [ -z "$TRAVIS_TAG" ] || vsce publish -p $VSCE_TOKEN
    vsce package || true
    popd
    mkdir upload
    cp -v integration/vscode/ada/*.vsix upload/
}

sed -i -e "s/VERSION/$TAG/g" integration/travis/bintray.json
${PLATFORM}_deploy
