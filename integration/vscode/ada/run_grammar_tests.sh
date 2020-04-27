# Very crude driver for the grammar tests.
#
# Usage:
#     ./run_grammar_tests.sh  [path_to_test]
#
# Where path_to_test is the path to one testcase. If omitted,
# process all tests under testsuite_grammar

testpath=$1

run_test(){
    dir=$1
    _err=0

    ada_files=`find $dir -name "*.ad?"`
    gpr_files=`find $dir -name "*.gpr"`

    if [ "$ada_files" != "" ]; then
      ./node_modules/.bin/vscode-tmgrammar-snap -g syntaxes/ada.tmLanguage.json \
        -s source.ada \
        -t "$dir/*.ad?" || _err=1
    fi

    if [ "$gpr_files" != "" ]; then
      ./node_modules/.bin/vscode-tmgrammar-snap -g syntaxes/gpr.tmLanguage.json \
        -s source.gpr \
        -t "$dir/*.gpr" || _err=1
    fi

    return $_err
}

error=0

if [ "$testpath" != "" ]; then
   run_test $testpath || error=1
else
   for dir in `ls testsuite_grammar`; do
      run_test testsuite_grammar/$dir || error=1
   done
fi
    
exit $error