#!/usr/bin/env python

""" Development utility to grab the most recent trace files and make
    a testcase out of them.

To use this, do

  traces_to_test.py  name_of_test  [path_to_traces_file]  > name_of_test_driver.json

"""

import sys
import os
import json
from json_transformations import python_to_protocol_string, traces_to_test

testname = sys.argv[1]
root = os.path.abspath(os.path.abspath(os.path.join('testsuite', 'ada_lsp', testname)))

if len(sys.argv) > 2:
    inout_file = sys.argv[2]
else:
    als_dir = os.path.join(os.path.expanduser('~'), '.als')
    inout_file = os.path.join(als_dir, 'inout.txt')

test = traces_to_test(inout_file, root)

print("create test directory {}".format(root))
os.makedirs(root, exist_ok=True)

test_yaml_file = os.path.join(root, 'test.yaml')
print("generating {}".format(test_yaml_file))
with open(test_yaml_file, "w") as f:
    f.write("title: '{}'\n".format(testname))

testfile = os.path.join(root, 'test.json')
print("generating {}".format(testfile))

with open(testfile, "w") as f:
   f.write(json.dumps(test, indent=3))
