#!/usr/bin/env python

""" Development utility to grab the most recent trace files and make
    a testcase out of them.

To use this, do

   traces_to_test.py    path_to_project_root    > name_of_test_driver.json

"""

import sys
import os
import json
from json_transformations import python_to_protocol_string, traces_to_test

als_dir = os.path.join(os.path.expanduser('~'), '.als')
inout_file = os.path.join(als_dir, 'inout.txt')
root = os.path.abspath(sys.argv[1])
test = traces_to_test(inout_file, root)
print json.dumps(test, indent=3)
