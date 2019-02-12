#!/bin/bash -e

PLATFORM=$1
DIR=integration/vscode/ada/$PLATFORM
LIB=/Projects/ada-language-server/adalib/lib
GNAT=/c/GNAT/

function add_dll()
{
    mkdir $DIR/$1
    cp $2/$1.dll $DIR/$1
    cat > $DIR/$1/$1.manifest <<EOF
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
  <assemblyIdentity
     name="$1"
     version="1.0.2018.0523"
     processorArchitecture="amd64" type="win32" />
  <file name="$1.dll" />
</assembly>
EOF

    cat >>$DIR/ada_language_server.exe.manifest <<EOF
<dependency>
  <dependentAssembly>
    <assemblyIdentity
       type="win32" name="$1"
       version="1.0.2018.0523"
       processorArchitecture="amd64"
       language="*" />
  </dependentAssembly>
</dependency>
EOF
}

cat > $DIR/ada_language_server.exe.manifest <<EOF
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1"
  manifestVersion="1.0">
<assemblyIdentity version="1.0.2018.0523"
  processorArchitecture="amd64"
  name="ada_language_server"
  type="win32"/>
EOF

add_dll libadalang         $LIB/libadalang.relocatable
add_dll libgnatcoll        $LIB/gnatcoll.relocatable
add_dll libgnatcoll_gmp    $LIB/gnatcoll_gmp.relocatable
add_dll libgnatcoll_iconv  $LIB/gnatcoll_iconv.relocatable
add_dll liblangkit_support $LIB/langkit_support.relocatable
add_dll libxmlada_dom           $GNAT/bin/
add_dll libxmlada_input_sources $GNAT/bin/
add_dll libxmlada_sax           $GNAT/bin/
add_dll libxmlada_schema        $GNAT/bin/
add_dll libxmlada_unicode       $GNAT/bin/
add_dll libgcc_s_seh-1          $GNAT/bin/
add_dll libgnarl-2018           $GNAT/bin/
add_dll libgnat-2018            $GNAT/bin/
add_dll libgpr                  $GNAT/bin/
add_dll libgmp-10               /mingw64/bin/
add_dll libiconv-2              /mingw64/bin/

cat >> $DIR/ada_language_server.exe.manifest <<EOF
</assembly>
EOF

pushd integration/vscode/ada/
7z a ../../../$PLATFORM.zip $PLATFORM
popd
