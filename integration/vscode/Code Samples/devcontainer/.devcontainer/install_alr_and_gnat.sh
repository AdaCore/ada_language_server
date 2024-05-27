#!/bin/sh

set -e -x

curl -L -O https://github.com/alire-project/alire/releases/download/v2.0.1/alr-2.0.1-bin-x86_64-linux.zip
unzip alr-2.0.1-bin-x86_64-linux.zip
rm -f alr-2.0.1-bin-x86_64-linux.zip
echo export PATH=$(realpath bin):'$PATH' >> ~/.profile
export PATH=$(realpath bin):$PATH
alr --non-interactive toolchain --select gnat_native gprbuild
