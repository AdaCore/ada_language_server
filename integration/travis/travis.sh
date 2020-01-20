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

function osx_script()
{
    export LIBRARY_PATH=/usr/local/lib        # To find GMP
    make OS=osx LIBRARY_TYPE=relocatable all
    integration/travis/deploy.sh darwin
}

${TRAVIS_OS_NAME}_$1
