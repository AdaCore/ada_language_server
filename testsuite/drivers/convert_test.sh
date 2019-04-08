testjson=$1
dir=`dirname $testjson`
basename=` basename $testjson  | rev | cut -b 6- | rev  `
./.obj/tester/tester-run $testjson
python testsuite/drivers/trace_to_test.py /home/setton/.als/in.txt /home/setton/.als/out.txt $dir > $dir/$basename.test
