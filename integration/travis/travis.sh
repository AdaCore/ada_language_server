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
    docker run -i -t lsp /bin/bash -c \
 'tar xzf /tmp/lsp.tar.gz && make -C ada_language_server LIBRARY_TYPE=relocatable'

}

${TRAVIS_OS_NAME}_$1
