FROM debian
ENV PATH=/opt/gnat/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN apt-get update && apt-get install -y \
 libx11-xcb1 \
 fontconfig \
 dbus \
 wget \
 make \
 libc-dev \
 git \
 libgmp-dev \
 && git clone https://github.com/AdaCore/gnat_community_install_script.git \
 && wget --no-verbose -O ./gnat-install \
        http://mirrors.cdn.adacore.com/art/5b0d7bffa3f5d709751e3e04 \
 && sh gnat_community_install_script/install_package.sh \
      ./gnat-install /opt/gnat \
 && wget -O- https://dl.bintray.com/reznikmm/test_lal_repo/libadalang.tar.gz \
  | tar xzf - -C /opt/gnat \
 && gprinstall --uninstall gnatcoll \
 && rm -rf gnat-install \
 && apt-get purge -y --auto-remove fontconfig dbus wget git libx11-6 libx11-xcb1 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
COPY lsp.tar.gz /tmp/
