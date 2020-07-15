#!/usr/bin/env python

""" Development utility to replay the most recent trace file, and output it
as protocol strings. This assumes that the trace file is in ~/.als/in.txt

To use this, do

   replay.py > log

then you can replay at will with

  ada_language_server < log
"""

import os
from json_transformations import python_to_protocol_string, traces_to_test

als_dir = os.path.join(os.path.expanduser('~'), '.als')
inout_file = os.path.join(als_dir, 'inout.txt')
test = traces_to_test(inout_file, None, True)
result = ""
for x in test:
    if "send" in x:
        result += python_to_protocol_string([x["send"]["request"]])
print(result + "\r")
