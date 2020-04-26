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
   ./node_modules/.bin/vscode-tmgrammar-snap -g syntaxes/ada.tmLanguage.json \
     -s source.ada \
     -t "$dir/*.ad?"
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