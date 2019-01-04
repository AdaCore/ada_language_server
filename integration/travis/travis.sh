#!/bin/bash -e

function linux_before_install()
{
    cp -R integration/travis /tmp/
    cd ..
    tar --exclude=.git \
        -c -z -f /tmp/travis/lsp.tar.gz ada_language_server
    docker build --tag lsp /tmp/travis
}

function linux_script()
{
    docker run -i -t -v$(pwd)/upload:/upload lsp /bin/bash -c \
 'tar xzf /tmp/lsp.tar.gz && make -C ada_language_server LIBRARY_TYPE=relocatable deploy'

}

GNAT_INSTALLER=gnat-community-2018-20180523-x86_64-darwin-bin.dmg
INSTALL_DIR=$PWD/../gnat

function osx_before_install()
{
    echo INSTALL_DIR=$INSTALL_DIR
    git clone https://github.com/AdaCore/gnat_community_install_script.git
    wget -nv -O $GNAT_INSTALLER \
        http://mirrors.cdn.adacore.com/art/5b071da0c7a447e573318b01
    sh gnat_community_install_script/install_package.sh \
        $GNAT_INSTALLER $INSTALL_DIR
    $INSTALL_DIR/bin/gprinstall --uninstall gnatcoll
    wget -nv -O- https://dl.bintray.com/reznikmm/libadalang/libadalang-master-osx.tar.gz \
        | tar xzf - -C $INSTALL_DIR
}

function osx_script()
{
    export PATH=$INSTALL_DIR/bin:$PATH
    make OS=osx LIBRARY_TYPE=relocatable all
    integration/travis/deploy.sh darwin
}

${TRAVIS_OS_NAME}_$1
