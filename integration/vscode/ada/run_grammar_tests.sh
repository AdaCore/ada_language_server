# Very crude driver for the grammar tests.
#
# Usage:
#     ./run_grammar_tests.sh  [path_to_test]
#
# Where path_to_test is the path to one testcase. If omitted,
# process all tests under testsuite_grammar

update=
for arg in $*; do
    if [ "$arg" = "--update" ]; then
        update="--updateSnapshot"
    else
        testpath="$arg"
    fi
done

run_test(){
    dir=$1
    _err=0

    ada_files=`find $dir -name "*.ad?"`
    gpr_files=`find $dir -name "*.gpr"`

    if [ "$ada_files" != "" ]; then
      # Test ada files with both simple and advanced classifiers
      for syntax in syntaxes advanced ; do
         # Grab the .snap files
         for src in $ada_files ; do
            [ -f $src.snap.$syntax ] && mv $src.snap.$syntax $src.snap
         done

         # Run the test. Use a non-existing language config, otherwise the tool
         # automatically loads the grammar configured in the package.json
         echo -n "[Ada $syntax]\t"
         ./node_modules/.bin/vscode-tmgrammar-snap -g $syntax/ada.tmLanguage.json \
           --config "none" \
           -s source.ada \
           $update \
           "$dir/*.ad?" || _err=1

         # Copy back any generated snap files
         for snap in $dir/*.snap ; do
            mv $snap $snap.$syntax
         done
      done
    fi

    if [ "$gpr_files" != "" ]; then
      echo -n "[GPR]\t\t"
      ./node_modules/.bin/vscode-tmgrammar-snap -g syntaxes/gpr.tmLanguage.json \
        -s source.gpr \
        $update \
        "$dir/*.gpr" || _err=1
    fi

    return $_err
}

error=0

if [ "$testpath" != "" ]; then
    run_test $testpath || error=1
else
    for dir in `find test/TestWorkspace/highlighing -type d -depth 1`; do
        run_test $dir || error=1
    done
fi

exit $error
