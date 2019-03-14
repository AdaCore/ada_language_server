FROM debian
ENV PATH=/opt/gnat/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
ENV USER=travis
RUN mkdir -p /tmp/ada_language_server
COPY . /tmp/ada_language_server/
RUN apt-get update && apt-get install -y \
 libx11-xcb1 \
 fontconfig \
 dbus \
 wget \
 make \
 libc-dev \
 git \
 nodejs-legacy \
 chrpath \
 libgmp-dev \
 && git clone https://github.com/AdaCore/gnat_community_install_script.git \
 && sh gnat_community_install_script/install_package.sh \
      /tmp/ada_language_server/gnat-install /opt/gnat \
 && gprinstall --uninstall gnatcoll \
 && wget -nv -O- https://dl.bintray.com/reznikmm/libadalang/libadalang-master-linux.tar.gz \
  | tar xzf - -C /opt/gnat \
 && rm -rf /tmp/ada_language_server/gnat-install \
 && apt-get purge -y --auto-remove fontconfig dbus wget git libx11-6 libx11-xcb1 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
