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
vss_text
vss_extra
xdiff
"

# Pins repo names (crate name by default)

repo_gnatcoll=gnatcoll-core
repo_lal_refactor=lal-refactor
repo_langkit_support=langkit
repo_libadalang_tools=libadalang-tools
repo_libgpr=gprbuild
repo_libgpr2=gpr
repo_prettier_ada=prettier-ada
repo_vss_text=vss-text
repo_vss_extra=vss-extra
repo_xdiff=xdiff

# Pins branches (master by default)

branch_lal_refactor=main
branch_gnatformat=main
branch_libgpr2=main
branch_prettier_ada=main
branch_vss_text=main
branch_vss_extra=main
branch_xdiff=main

# Repository URLs can be overriden (e.g. to personal forks for experimentation)
# url_langkit_support=https://github.com/<my-github-login>/langkit.git

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

      url_var=url_$crate
      url_override=${!url_var}
      URL=${url_override:-"https://github.com/AdaCore/${repo:-$crate}.git"}

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
      if [ -z "$commit" ]; then
         # If no specific commit was requested, a branch is used. The previous
         # checkout command would simply switch to it but not update it from
         # remote. So let's do that update.
         git -C "subprojects/$crate" pull origin "${branch:-master}"
      fi

      if [ -f "subprojects/$crate.toml" ]; then
         cp -v "subprojects/$crate".toml "subprojects/$crate/alire.toml"
      fi

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

# A temporary file for langkit environment
LANGKIT_SETENV=$PWD/subprojects/libadalang/setenv.sh

# Build langkit which is required to generate libadalang.
function build_langkit_raw() {
   (
      # Use a sub-shell to preserve the parent PWD
      cd subprojects/langkit_support

      echo "GPR_PROJECT_PATH=$GPR_PROJECT_PATH"

      sed -i.bak -e 's/GPR_BUILD/GPR_LIBRARY_TYPE/' ./langkit/libmanage.py
      pip install .

      python manage.py make --no-mypy --generate-auto-dll-dirs \
         --library-types=relocatable --gargs="-m -j0 -vh"

      # Export the environment needed to use langkit into a file for later
      # usage
      python manage.py printenv >"$LANGKIT_SETENV"

      if [[ $NODE_ARCH_PLATFORM == "x64/win32" ]]; then
         # Fix setenv.sh to be bash script for MSYS2 by replacing
         #  1) C:\ -> /C/  2) '\' -> '/' and ';' -> ':' 3) ": export" -> "; export"
         #
         # Only do this on the PATH environment variable which MSYS2/Cygwin
         # automatically converts to Windows paths. Other variables such as
         # PYTHONPATH must be left in Windows format to be usable by Python
         # subprocresses.
         #
         # See https://www.msys2.org/docs/filesystem-paths/
         sed -i -e '/^PATH=/s#\([A-Z]\):\\#/\1/#g' -e '/^PATH=/y#\\;#/:#' -e '/^PATH=/s/: export /; export /' "$LANGKIT_SETENV"
      fi

      cat "$LANGKIT_SETENV"
   )
}

# On macOS, there are two issues with gprbuild:
#
# 1. The linker arguments for rpath produce a leading space into paths:
#    "-Wl,-rpath, @executable_path/...". This makes the dynamic library loader
#    unable to use those rpaths at runtime.
# 2. The linker uses "@executable_path" which applies in the context of an
#    exectuable but not in a context where the library is loaded directly, which
#    is precisely the case we want when we do `import liblktlang` in Python.
#    Instead, "@loader_path" should be used.
#
# This function applies a workaround by removing the leading space from rpath
# entries, and replacing @executable_path with @loader_path.
function fix_dylib_rpaths() {
   lib=$1
   # Log the full output of otool for debugging
   otool -l "$lib"

   # First fix paths with a leading space
   paths_with_space=$(otool -l "$lib" | grep -A2 LC_RPATH | grep "path  " | awk '{ print $2 }')
   for p in $paths_with_space; do
      install_name_tool -rpath " $p" "${p/@executable_path/@loader_path}" "$lib"
   done
   # Then replace @executable_path with @loader_path in all paths (there can be
   # ones without a leading space, hence doing 2 passes)
   paths_with_exec_path=$(otool -l "$lib" | grep -A2 LC_RPATH | grep "@executable_path" | awk '{ print $2 }')
   for p in $paths_with_exec_path; do
      install_name_tool -rpath "$p" "${p/@executable_path/@loader_path}" "$lib"
   done
}

# Run build_langkit_raw in Alire environment
function build_langkit() {
   (
      # The langkit build will try to import liblktlang at the end. For that
      # to work, all dependency libraries must be visible on
      # [[DY]LD_LIBRARY_]PATH. This function takes care of that.
      add_unpinned_deps_dlls_to_runtime_path

      # We use 'alr exec' to benefit from Alire setting up GPR_PROJECT_PATH with
      # all the dependencies.
      alr exec bash -- -x "$0" build_langkit_raw
   )
}

# This function adds the paths of DLLs from GCC installation and the Alire deps
# in alire/cache/dependencies (i.e. Alire deps that haven't been pinned to
# local checkouts) to the runtime environement so that they may be loaded in
# cases where the DLLs we build don't have appropriate RPATH entries.
#
# This is necessary on Windows and macOS, so PATH and DYLD_LIBRARY_PATH are
# used.
#
# However, on macOS we're now using fix_dylib_rpaths to correct the RPATH
# entries, so DYLD_LIBRARY_PATH should no longer be necessary. But we keep it
# for good measure.
function add_unpinned_deps_dlls_to_runtime_path() {
   ADALIB=$(alr exec gcc -- -print-libgcc-file-name)

   if [[ $NODE_ARCH_PLATFORM == "x64/win32" ]]; then
      ADALIB=$(cygpath -u "$ADALIB")
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
}

# This is a utility function to run a command line within an environment where
# the Langkit 'lkt' library is available.
#
# e.g. lkt_run python -c 'import liblktlang'
#
# Currently it is unused because on macOS spawning processes through a chain of
# lkt_run --> alr --> bash does not allow inheriting DYLD_LIBRARY_PATH and
# doesn't allow the libraries to be found.
function lkt_run() {
   python -m langkit.scripts.lkm run --config "$ROOT/subprojects/langkit_support/lkt/langkit.yaml" -- "${@}"
}

# Build ALS with alire
function build_als() {
   add_unpinned_deps_dlls_to_runtime_path

   # Check that we can use langkit successfully
   (
      source "$LANGKIT_SETENV"

      # Log environments for debugging
      python -c 'import os; print("\n".join(f"{k}={v}" for k, v in os.environ.items()))'
      alr exec python -- -c 'import os; print("\n".join(f"{k}={v}" for k, v in os.environ.items()))'

      # On Windows it is not enough to source the langkit env and unpinned
      # deps. The libraries of pinned Alire dependencies (not under
      # alire/cache/dependencies) must also be made visible by calling 'alr exec'
      alr exec python -- -c 'import liblktlang; print("Imported liblktlang successfully")'
   )

   # We use 'alr exec' to benefit from Alire setting up GPR_PROJECT_PATH with
   # all the dependencies.
   LIBRARY_TYPE=static STANDALONE=no alr exec make -- "VERSION=$TAG" "GPRBUILD_CARGS=-m -vh" all
}

function test_als() {
   pip install -r "$ROOT/testsuite/requirements-ci.txt"
   export ALS="$ROOT/.obj/server/ada_language_server"
   alr exec python -- "$ROOT/testsuite/testsuite.py" --failure-exit-code 0 --show-error-output
   e3-testsuite-report --xunit-output xunit.xml out/new
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
   pin_crates
   build_langkit
   build_als
   fix_rpath
   strip_debug
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

fix_dylib_rpaths)
   fix_dylib_rpaths "$2"
   ;;

*)
   echo "Unrecognized step: $STEP"
   exit 1
   ;;
esac
