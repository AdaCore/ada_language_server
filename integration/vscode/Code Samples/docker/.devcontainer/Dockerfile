# Put GNAT Pro .tar.gz into this directory to be installed
# otherwise it will install GNAT from Ubuntu.

FROM ubuntu:22.04

COPY gnatpro-*bin.tar.gz /tmp/

RUN set -xe \
    && DEBIAN_FRONTEND=noninteractive apt-get update -y \
    && apt-get install -y \
         --no-install-recommends \
        make \
        libc6-dev \
    && find /tmp/gnatpro-*bin.tar.gz -size +1 -exec tar xaf {} -C /tmp \; -delete \
    && rm -rf /tmp/gnatpro-*-bin.tar.gz \
    && find /tmp/ -name doinstall -execdir {} /usr/local \; \
    && rm -rf /tmp/gnatpro-* \
    && if [ ! -f /usr/local/bin/gnat ] ; \
       then apt-get install -y gprbuild gnat gdb ; fi \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get purge --auto-remove \
    && apt-get clean

