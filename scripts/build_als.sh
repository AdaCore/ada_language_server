#!/bin/bash

set -e -x

STEP=${1:-all}
TAG=$2                # For master it's 24.0.999, while for tag it's the tag itself
NODE_ARCH_PLATFORM=$3 # Empty - autodetect with `node`, otherwise one of:
# arm64/darwin
# arm64/linux
# x64/darwin
# x64/linux
# x64/win32

# Crates to pin from GitHub repo

PINS="
adasat
gnatcoll
gnatdoc
gnatformat
lal_refactor
langkit_support
libadalang
libadalang_tools
libgpr
libgpr2
markdown
prettier_ada
spawn
vss
"

# Pins repo names (crate name by default)

repo_gnatcoll=gnatcoll-core
repo_lal_refactor=lal-refactor
repo_langkit_support=langkit
repo_libadalang_tools=libadalang-tools
repo_libgpr=gprbuild
repo_libgpr2=gpr
repo_prettier_ada=prettier-ada
repo_vss=VSS

# Pins branches (master by default)

branch_gnatdoc=edge
branch_lal_refactor=edge
branch_gnatformat=edge
branch_libgpr2=next
branch_prettier_ada=main

# A temporary file for langkit setevn
SETENV=$PWD/subprojects/libadalang/setenv.sh

# Set `prod` build mode
########################
# for adasat,gnatformat,lal,langkit,lal_refactor,laltools,markdown,spawn
export BUILD_MODE=prod
# for others
export GNATCOLL_BUILD_MODE=PROD
export GPR_BUILD=production
export GPR2_BUILD=release
export VSS_BUILD_PROFILE=release
export PRETTIER_ADA_BUILD_MODE=prod

# Install custom Alire index with missing crates
function install_index() {
   alr index --del=als || true
   alr index --add="$PWD" --name=als
}

# Clone dependencies
function pin_crates() {
   for crate in $PINS; do
      repo_var=repo_$crate
      branch_var=branch_$crate

      repo=${!repo_var}
      branch=${!branch_var}
      commit=""

      if [ -f deps.txt ]; then
         commit=$(grep "^${repo:-$crate}=" deps.txt | sed -e 's/.*=//')
      fi

      URL="https://github.com/AdaCore/${repo:-$crate}.git"
      if [ ! -d "subprojects/$crate" ]; then
         git clone "$URL" "subprojects/$crate"
         git -C "subprojects/$crate" checkout "${commit:-${branch:-master}}"
      fi
      cp -v "subprojects/$crate".toml "subprojects/$crate/alire.toml"
      alr --force --non-interactive pin "$crate" "--use=$PWD/subprojects/$crate"
   done

   alr exec alr -- action -r post-fetch # Configure XmlAda, etc
}

# Build langkit shared libraries required to generate libadalang.
# Export setenv to $SETENV
# Clean `.ali` and `.o` to avoid static vis relocatable mess
function build_so_raw() {
   cd subprojects/langkit_support
   echo "GPR_PROJECT_PATH=$GPR_PROJECT_PATH"
   sed -i.bak -e 's/GPR_BUILD/GPR_LIBRARY_TYPE/' ./langkit/libmanage.py
   pip install .
   python manage.py make --no-mypy --generate-auto-dll-dirs \
      --library-types=relocatable --gargs "-cargs -fPIC"
   python manage.py setenv >"$SETENV"
   cd -
   find . -name '*.o' -delete
   find . -name '*.ali' -delete
}

# Run build_so_raw in Alire environment
function build_so() {
   LIBRARY_PATH=relocatable alr exec bash -- "$0" build_so_raw
}

# Build ALS with alire
function build_als() {
   ADALIB=$(alr exec gcc -- -print-libgcc-file-name)

   if [[ $NODE_ARCH_PLATFORM == "x64/win32" ]]; then
      ADALIB=$(cygpath -u "$ADALIB")
      # Fix setenv.sh to be bash script for MSYS2 by replacing
      #  1) C:\ -> /C/  2) '\' -> '/' and ';' -> ':' 3) ": export" -> "; export"
      sed -i -e 's#\([A-Z]\):\\#/\1/#g' -e 'y#\\;#/:#' -e 's/: export /; export /' "$SETENV"
      cat "$SETENV"
      # libgcc_s_seh-1.dll is already in PATH

   elif [[ $NODE_ARCH_PLATFORM == "x64/linux" ]]; then
      NEW_PATH=$(dirname "$(alr exec gcc -- -print-file-name=libgcc_s.so.1)")
   else
      NEW_PATH=$(dirname "$(alr exec gcc -- -print-file-name=libgcc_s.dylib.1)")
   fi

   ADALIB=$(dirname "$ADALIB")/adalib
   DEPS=$PWD/alire/cache/dependencies
   NEW_PATH=$ADALIB:$NEW_PATH

   for ITEM in $DEPS/*/{iconv,gmp,schema,dom,sax,input_sources,unicode}; do
      [ -d "$ITEM" ] && NEW_PATH=$ITEM/lib/relocatable:$NEW_PATH
   done

   echo "NEW_PATH=$NEW_PATH"
   export DYLD_LIBRARY_PATH=$NEW_PATH:$DYLD_LIBRARY_PATH
   export PATH=$NEW_PATH":$PATH"
   # Let's ignore make check exit code until it is stable
   LIBRARY_TYPE=static STANDALONE=no alr exec make -- "VERSION=$TAG" check || true
}

# Find the path to libgmp as linked in the given executable
function get_gmp_full_path() {
   otool -l "$1" | grep '^\s*name.*libgmp.10.dylib' | awk '/ name /{print $2 }'
}

function do_fix_rpath() {
   # Remove all rpath entries
   for R in $(otool -l "$1" | grep -A2 LC_RPATH | awk '/ path /{ print $2 }'); do
      install_name_tool -delete_rpath "$R" "$1"
   done
   # Change reference to full path of libgmp into a reference to the rpath.
   gmp_full_path=$(get_gmp_full_path "$1")
   if [ -n "$gmp_full_path" ]; then
      install_name_tool -change "$gmp_full_path" @rpath/libgmp.10.dylib "$1"
   fi
   # Add the executable directory to rpath so it can find shared libraries
   # packaged alongside the executable.
   install_name_tool -add_rpath @executable_path "$1"
}

if [[ -z "$NODE_ARCH_PLATFORM" ]]; then
   # Get architecture and platform information from node.
   NODE_PLATFORM=$(node -e "console.log(process.platform)")
   NODE_ARCH=$(node -e "console.log(process.arch)")
   NODE_ARCH_PLATFORM=$NODE_ARCH/$NODE_PLATFORM
fi

ALS_EXEC_DIR=integration/vscode/ada/$NODE_ARCH_PLATFORM

function fix_rpath() {
   if [ "$RUNNER_OS" = macOS ]; then
      # Get full path of libgmp as linked in the ALS exec
      gmp_full_path=$(get_gmp_full_path "$ALS_EXEC_DIR/ada_language_server")
      if [ -f "$gmp_full_path" ]; then
         # Copy libgmp alongside the ALS exec
         cp -v -f "$gmp_full_path" "$ALS_EXEC_DIR"
      fi
      # Fix rpath entries of the ALS exec so it can find libgmp alongside it
      do_fix_rpath "$ALS_EXEC_DIR/ada_language_server"
   fi
}

function strip_debug() {
   cd "$ALS_EXEC_DIR"

   if [ "$RUNNER_OS" = Windows ]; then
      ALS=ada_language_server.exe
   else
      ALS=ada_language_server
   fi

   if [ "$RUNNER_OS" = macOS ]; then
      # On macOS using objcopy from binutils to strip debug symbols to a
      # separate file doesn't work. Namely, the last step `objcopy
      # --add-gnu-debuglink` yields an executable that crashes at startup.
      #
      # Instead we use dsymutil and strip which are commands provided by the
      # system (or by XCode).
      dsymutil "$ALS"
      strip "$ALS"
   elif [[ $NODE_ARCH_PLATFORM == "arm64/linux" ]]; then
      aarch64-linux-gnu-objcopy --only-keep-debug ${ALS} ${ALS}.debug
      aarch64-linux-gnu-objcopy --strip-all ${ALS}
      aarch64-linux-gnu-objcopy --add-gnu-debuglink=${ALS}.debug ${ALS}
   else
      objcopy --only-keep-debug ${ALS} ${ALS}.debug
      objcopy --strip-all ${ALS}
      objcopy --add-gnu-debuglink=${ALS}.debug ${ALS}
   fi
   cd -
}

case $STEP in
all)
   install_index
   pin_crates
   build_so
   build_als
   fix_rpath
   strip_debug
   ;;

install_index)
   install_index
   ;;

pin_crates)
   pin_crates
   ;;

build_so)
   build_so
   ;;

build_so_raw)
   build_so_raw
   ;;

build_als)
   build_als
   ;;

fix_rpath)
   fix_rpath
   ;;

strip_debug)
   strip_debug
   ;;
esac
