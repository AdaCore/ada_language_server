#!/usr/bin/env python

"""
Development utility to replay an ALS session based on a log file containing
the ALS.IN and ALS.OUT traces, and output it as protocol strings.

Usage: replay.py --log-file <path_to_log_file> --output-file <output_file>
"""

import argparse
from json_transformations import python_to_protocol_string, traces_to_test


argParser = argparse.ArgumentParser()
argParser.add_argument(
    "-l",
    "--log-file",
    help="Path to the log file containing ALS.IN and ALS.OUT traces.",
    required=True,
)
argParser.add_argument(
    "-o",
    "--output-file",
    help="Path to the output file containing all the ALS requests retrieved "
    + "from the log file. Then you can replay a session like this: "
    + "ada_language_server < <output_file>",
    required=False,
)
args = argParser.parse_args()

inout_file = args.log_file
output_file = args.output_file
test = traces_to_test(inout_file, None, True)
result = ""
for x in test:
    if "send" in x:
        result += python_to_protocol_string([x["send"]["request"]])

# Print on stdout if no output file has been specified
if output_file:
    with open(output_file, "w") as file:
        file.write(result + "\r")
else:
    print(result + "\r")
