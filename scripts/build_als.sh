#!/bin/bash

# Stop on errors
set -e
# Make execution verbose
set -x

STEP=${1:-all}
TAG=$2                # For master it's 24.0.999, while for tag it's the tag itself
NODE_ARCH_PLATFORM=$3 # Empty - autodetect with `node`, otherwise one of:
# arm64/darwin
# arm64/linux
# x64/darwin
# x64/linux
# x64/win32

MYPATH=$(realpath "$0")
MYDIRPATH=$(dirname "$MYPATH")
ROOT=$(dirname "$MYDIRPATH")
VENV_PATH="$ROOT/.venv"

# Switch to the root of the ALS repository
cd "$ROOT"

# Configure Alire to use a local cache because we will need to copy shared
# libraries from there
alr settings --global --set dependencies.shared false

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

function activate_venv() {
   [ -d "$VENV_PATH" ] || python -m venv "$VENV_PATH"
   case "$NODE_ARCH_PLATFORM" in
   *win32*)
      subdir=Scripts
      ;;
   *)
      subdir=bin
      ;;
   esac
   source "$VENV_PATH/$subdir/activate"
}

# Install custom Alire index with missing crates
function install_index() {
   alr index --del=als || true
   alr index --add="$PWD" --name=als
}

# Clone dependencies
function pin_crates() {
   # Many dependencies are not yet available as Alire crates in the community
   # index.  To work around that we have defined crates for those projects at
   # subprojects/*.toml. For each project, we check out its repository and
   # place the corresponding .toml file at the root. This includes the crate
   # file for the ALS itself.

   # Place the ALS toml at the root
   cp -v "subprojects/als.toml" "$PWD/alire.toml"

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
         # If the checkout doesn't exist, clone
         git clone "$URL" "subprojects/$crate"
      else
         # This script makes some changes in files of dependency checkouts to
         # make the build work with Alire. Stash them to avoid interference
         # with the next Git commands.
         git -C "subprojects/$crate" stash
         # If the checkout exists, reuse the directory but fetch the new history
         git -C "subprojects/$crate" fetch origin
      fi
      git -C "subprojects/$crate" checkout "${commit:-${branch:-master}}"
      cp -v "subprojects/$crate".toml "subprojects/$crate/alire.toml"

      # Instead of calling `alr pin` for each crate, it's more efficient to
      # append the necessary text in alire.toml and call `alr update` once at
      # the end.
      cat >>"$PWD/alire.toml" <<EOF
[[pins]]
$crate = { path='subprojects/$crate' }

EOF
   done

   alr --force --non-interactive update

   alr exec alr -- action -r post-fetch # Configure XmlAda, etc
}

# Build langkit which is required to generate libadalang.
#
# The idea is to build langkit in the form of an installable wheel that
# contains all the dynamic libraries of dependencies such that Python can
# easily find everything at runtime. This avoids the security limitation of
# macOS not propagating DYLD_LIBRARY_PATH to child processes.
function build_langkit_raw() {
   (
      # Use a sub-shell to preserve the parent PWD
      cd subprojects/langkit_support

      echo "GPR_PROJECT_PATH=$GPR_PROJECT_PATH"

      # Adjust the scenario variable for libgpr2
      sed -i.bak -e 's/GPR_BUILD/GPR_LIBRARY_TYPE/' ./langkit/libmanage.py

      case "$NODE_ARCH_PLATFORM" in
      *darwin*)
         # Install e3-binarydata which is needed for packaging langkit as a wheel
         pip install git+https://github.com/AdaCore/e3-binarydata.git#egg=e3-binarydata
         ;;
      esac

      # Install base langkit support Python library
      pip install .

      # Create a directory where langkit will be installed
      prefix_dir=$PWD/lkt-install
      rm -rf "$prefix_dir"
      mkdir -p "$prefix_dir"
      # Create a temporary dir used during the creation of the langkit wheel
      tmp_dir=$PWD/lkt-tmp
      rm -rf "$tmp_dir"
      mkdir -p "$tmp_dir"
      # Create a directory where all shared libraries that langkit depends on will
      # be copied prior to the creation of the wheel
      dep_lib_dir="$PWD/dep-libs"
      rm -rf "$dep_lib_dir"
      mkdir -p "$dep_lib_dir"

      # This builds langkit, as well as all the library projects of the dependencies
      python manage.py build-langkit-support \
         --library-types=relocatable
      python manage.py install-langkit-support "$prefix_dir" \
         --library-types=relocatable

      # Next build and install langkit
      (cd contrib/lkt && ./manage.py make --library-types=relocatable --disable-all-mains)
      (cd contrib/lkt && ./manage.py install "$prefix_dir" \
         --library-types=relocatable --disable-all-mains)

      case "$NODE_ARCH_PLATFORM" in
      *darwin*)
         # on macOS, we want to copy all dependency libraries into the wheel to
         # avoid relying on DYLD_LIBRARY_PATH which has security restrictions.

         # First we copy the libraries from the GNAT installation.
         # Find the gnat installation directory
         gnat_prefix=$(dirname "$(dirname "$(which gnat)")")
         # Copy the libraries. Exclude paths with dSYM on macOS because those have .dylib homonyms with debug symbols.
         find "$gnat_prefix" -name "${OS_LIB_PREFIX}gnarl*${OS_LIB_EXT}" -not -path "*dSYM*" -exec cp -v {} "$dep_lib_dir" \;
         find "$gnat_prefix" -name "${OS_LIB_PREFIX}gnat*${OS_LIB_EXT}" -not -path "*dSYM*" -exec cp -v {} "$dep_lib_dir" \;

         # Alire projects are either in the als/alire/cache directory, or in
         # als/subprojects.
         find ../../alire/cache/dependencies .. -name "${OS_LIB_PREFIX}*${OS_LIB_EXT}" -exec cp -v {} "$dep_lib_dir" \;
         ;;
      esac

      # Finally build the wheel
      pip install wheel # necessary to make python setup.py bdist_wheel work
      (cd contrib/lkt && ./manage.py create-wheel \
         --verbosity=debug \
         --with-python=python \
         "$prefix_dir" \
         "$tmp_dir" \
         "$dep_lib_dir" \
         "$prefix_dir")

      # And now install the resulting wheel into the Python environment
      pip install "$prefix_dir"/*.whl --force-reinstall

      case "$NODE_ARCH_PLATFORM" in
      *linux*)
         # On Linux shared libraries are not built with the right RPATH entries
         # to make them movable. So we delete the installed library and instead
         # use LD_LIBRARY_PATH to resolve it. See set_langkit_usage_env().
         find "$VENV_PATH" -name "liblktlang.so" -delete
         ;;
      esac
   )
}

# # This function modified PATH and LD_LIBRARY_PATH so that libraries built by
# # Alire become visible. In particular, this is needed when importing liblktlang
# # (langkit) in Python to be able to load all libraries.
# function setup_library_path() {
#    ADALIB=$(alr exec gcc -- -print-libgcc-file-name)

#    if [[ $NODE_ARCH_PLATFORM == "x64/win32" ]]; then
#       ADALIB=$(cygpath -u "$ADALIB")
#    elif [[ $NODE_ARCH_PLATFORM == "x64/linux" ]]; then
#       NEW_PATH=$(dirname "$(alr exec gcc -- -print-file-name=libgcc_s.so.1)")
#    else
#       NEW_PATH=$(dirname "$(alr exec gcc -- -print-file-name=libgcc_s.dylib.1)")
#    fi

#    ADALIB=$(dirname "$ADALIB")/adalib
#    DEPS=$PWD/alire/cache/dependencies
#    NEW_PATH=$ADALIB:$NEW_PATH

#    for ITEM in "$DEPS"/*/{iconv,gmp,schema,dom,sax,input_sources,unicode}; do
#       [ -d "$ITEM" ] && NEW_PATH=$ITEM/lib/relocatable:$NEW_PATH
#    done

#    echo "NEW_PATH=$NEW_PATH"
#    export DYLD_LIBRARY_PATH=$NEW_PATH:$DYLD_LIBRARY_PATH
#    export LD_LIBRARY_PATH=$NEW_PATH:$LD_LIBRARY_PATH
#    export PATH=$NEW_PATH":$PATH"
# }

# Run build_langkit_raw in Alire environment
function build_langkit() {
   # We use 'alr exec' to benefit from Alire setting up GPR_PROJECT_PATH with
   # all the dependencies.
   alr exec bash -- -x "$0" build_langkit_raw

   # Check if the langkit build was successful
   set_langkit_usage_env
   python -c 'import liblktlang'
}

function set_langkit_usage_env() {
   case "$NODE_ARCH_PLATFORM" in
   *linux*)
      lib_path_prefix=$PWD/subprojects/langkit_support/contrib/lkt/build/lib/relocatable
      export LD_LIBRARY_PATH="$lib_path_prefix/dev:$lib_path_prefix/prod:$LD_LIBRARY_PATH"
      ;;
   esac
}

# Build ALS with alire
function build_als() {
   set_langkit_usage_env

   # We use 'alr exec' to benefit from Alire setting up GPR_PROJECT_PATH with
   # all the dependencies.
   LIBRARY_TYPE=static STANDALONE=no alr exec make -- "VERSION=$TAG" all
}

function test_als() {
   pip install -r "$ROOT/testsuite/requirements-ci.txt"
   export ALS="$ROOT/.obj/server/ada_language_server"
   alr exec python -- "$ROOT/testsuite/testsuite.py"
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

case "$NODE_ARCH_PLATFORM" in
*darwin*)
   OS_LIB_PREFIX="lib"
   OS_EXE_EXT=""
   OS_LIB_EXT=".dylib"
   ;;
*win*)
   OS_LIB_PREFIX=""
   OS_EXE_EXT=".exe"
   OS_LIB_EXT=".dll"
   ;;
*)
   OS_LIB_PREFIX="lib"
   OS_EXE_EXT=""
   OS_LIB_EXT=".so"
   ;;
esac

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

# Always activate the Python venv first
activate_venv

case $STEP in
all)
   install_index
   pin_crates
   build_langkit
   build_als
   fix_rpath
   strip_debug
   test_als
   ;;

install_index)
   install_index
   ;;

pin_crates)
   pin_crates
   ;;

build_langkit)
   build_langkit
   ;;

build_langkit_raw)
   build_langkit_raw
   ;;

build_als)
   build_als
   ;;

test_als)
   test_als
   ;;

fix_rpath)
   fix_rpath
   ;;

strip_debug)
   strip_debug
   ;;

*)
   echo "Unrecognized step: $STEP"
   exit 1
   ;;
esac
