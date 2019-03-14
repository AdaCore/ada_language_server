#!/bin/bash
set -e -x
GNAT_INSTALLER=$HOME/cache/gnat-community-2018-20180528-x86_64-linux-bin
INSTALL_DIR=$HOME/gnat
export PATH=$INSTALL_DIR/bin:$PATH
mkdir -p $HOME/cache

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

function linux_before_gps()
{
    mkdir $HOME/adalib
    sudo apt-get update
    sudo apt-get install -y python-gobject-2-dev \
        python-dev \
        libclang-6.0-dev \
        pkg-config \
        libgtk-3-dev \
        libx11-xcb1 \
        fontconfig \
        dbus \
        libc-dev \
        wget \
        git \
        make \

    for J in gnat_community_install_script gps gnatcoll-db gnatcoll-bindings
    do
       git clone --depth=1 https://github.com/AdaCore/$J.git
    done

    for J in gtkada-master libadalang-stable; do
        wget -O- https://dl.bintray.com/reznikmm/libadalang/$J-linux.tar.gz | \
            tar xzf - -C $HOME/adalib
    done

    [ -f $GNAT_INSTALLER ] || wget -nv -O $GNAT_INSTALLER \
        http://mirrors.cdn.adacore.com/art/5b0d7bffa3f5d709751e3e04
    sh gnat_community_install_script/install_package.sh \
      $GNAT_INSTALLER $INSTALL_DIR
    gprinstall --uninstall gnatcoll
}

function linux_gps()
{
    export LIBRARY_TYPE=relocatable
    export ADA_PROJECT_PATH=$HOME/adalib/share/gpr:$HOME/adalib/lib/gnat:`pwd`/gnat
    echo ADA_PROJECT_PATH=$ADA_PROJECT_PATH

    for J in sql sqlite xref; do
        ARGS="-p -P gnatcoll-db/$J/gnatcoll_$J.gpr -XBUILD=PROD"
        gprbuild -j0 $ARGS
        gprinstall --prefix=$HOME/adalib $ARGS
    done

    ARGS="-p -P gnatcoll-bindings/python/gnatcoll_python.gpr -XBUILD=PROD"
    gprbuild -j0 $ARGS -cargs -I/usr/include/python2.7
    gprinstall --prefix=$HOME/adalib $ARGS

    cd gps/kernel/src
    python hooks.py
    cd ../..
    ./configure --with-clang=/usr/lib/llvm-6.0/lib/
    gprbuild -j0 -p -P lsp_client/gps_lsp_client.gpr -XBuild=Production
}


function osx_before_install()
{
    GNAT_INSTALLER=$HOME/cache/gnat-community-2018-20180523-x86_64-darwin-bin.dmg
    echo INSTALL_DIR=$INSTALL_DIR
    git clone https://github.com/AdaCore/gnat_community_install_script.git
    [ -f $GNAT_INSTALLER ] || wget -nv -O $GNAT_INSTALLER \
        http://mirrors.cdn.adacore.com/art/5b071da0c7a447e573318b01
    sh gnat_community_install_script/install_package.sh \
        $GNAT_INSTALLER $INSTALL_DIR
    $INSTALL_DIR/bin/gprinstall --uninstall gnatcoll
    wget -nv -O- https://dl.bintray.com/reznikmm/libadalang/libadalang-master-osx.tar.gz \
        | tar xzf - -C $INSTALL_DIR
}

function osx_script()
{
    make OS=osx LIBRARY_TYPE=relocatable all
    integration/travis/deploy.sh darwin
}

${TRAVIS_OS_NAME}_$1
