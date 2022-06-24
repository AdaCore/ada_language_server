#!/usr/bin/env sh

# This is a wrapper to run the Ada Language Sever with valgrind
# and all the needed arguments from the testsuite.

dir_path=$(dirname $0)
valgrind --quiet --tool=memcheck --leak-check=full --suppressions=$dir_path/leaks.supp $dir_path/../.obj/server/ada_language_server $@
