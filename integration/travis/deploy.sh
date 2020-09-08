#!/bin/bash -e

set -x

PLATFORM=$1
DIR=integration/vscode/ada/$PLATFORM
TAG=${TRAVIS_TAG:-latest}

function linux_deploy()
{
    LIB=$PWD/../gnat/lib

    cp --dereference $LIB/lib{adalang,langkit_support}.so $DIR
    cp --dereference $LIB/libgnatcoll{_gmp,_iconv,}.so.0 $DIR
    cp --dereference $LIB/libxmlada_{dom,input_sources,sax,schema,unicode}.so.2020 $DIR
    cp --dereference $LIB/libgpr.so $DIR

    chrpath -r '$ORIGIN/' $DIR/*

    cp $LIB/gcc/x86_64-pc-linux-gnu/9.3.1/adalib/lib{gnat,gnarl}-2020.so $DIR

    mkdir upload
    tar czvf upload/$PLATFORM-$TAG-dbg.tar.gz -C integration/vscode/ada/ $PLATFORM

    strip $DIR/*
    tar czvf upload/$PLATFORM-$TAG.tar.gz -C integration/vscode/ada/ $PLATFORM
}

function darwin_deploy()
{
    cp -v .obj/server/*.dylib $DIR

    mkdir upload
    tar czvf upload/$PLATFORM-$TAG.tar.gz -C integration/vscode/ada/ $PLATFORM
}

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

function vsix_deploy()
{
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
    make_change_log > CHANGELOG.md
    [ -z "$TRAVIS_TAG" ] || vsce publish -p $VSCE_TOKEN
    vsce package || true
    popd
    mkdir upload
    cp -v integration/vscode/ada/*.vsix upload/
}

sed -i -e "s/VERSION/$TAG/g" integration/travis/bintray.json
${PLATFORM}_deploy
