#!/usr/bin/env python

""" Development utility to grab the most recent trace files and make
    a testcase out of them.

To use this, do

   traces_to_test.py > name_of_test_driver.json

Note: this needs to be called from the PATH where the test project
resides.
"""

import os
import json
from json_transformations import python_to_protocol_string, traces_to_test

als_dir = os.path.join(os.path.expanduser('~'), '.als')
in_file = os.path.join(als_dir, 'in.txt')
out_file = os.path.join(als_dir, 'out.txt')
test = traces_to_test(in_file, out_file, os.getcwd())
print json.dumps(test, indent=3)
