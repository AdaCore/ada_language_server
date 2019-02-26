#!/bin/bash

# Terminate on any error
set -e

export BUILD_FOLDER=/Projects/ada-language-server
export ADALIB_DIR=$BUILD_FOLDER/adalib
export PATH=$ADALIB_DIR/bin:\
/c/GNAT/bin:\
/mingw64/bin:\
$PATH
export ADA_PROJECT_PATH=$ADALIB_DIR/share/gpr
export LIBRARY_TYPE=static
export CPATH=/mingw64/include
export LIBRARY_PATH=/mingw64/lib

function do_install()
{
  cd $BUILD_FOLDER
  pacman -S --noconfirm mingw-w64-x86_64-libiconv mingw-w64-x86_64-gmp
  curl -q -L -o libadalang-master-windows.zip \
    https://dl.bintray.com/reznikmm/libadalang/libadalang-master-windows.zip
  7z x libadalang-master-windows.zip -oadalib
}

function do_build()
{
  cd $BUILD_FOLDER
  make deploy USER=appveyor
}

do_$1
