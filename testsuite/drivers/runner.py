""" A driver to run a single test.
    Note: this driver is written independently of e3, for ease of integration
    into travis for now.
"""

import os
import subprocess
from distutils.spawn import find_executable

from json_transformations import (requests_to_protocol_string, SEND_KEY,
                                  RECEIVE_KEY, test_file_to_test,
                                  protocol_string_to_requests)


def run_single_test(testfile, path=None):
    """Run a single test pointed to by testfile.
       path is the path that $PWD should be substituted to in the test files;
       if unspecified, this will be inferred from testfile.

       return list of errors
    """

    # Compute the path of the test base
    if path is None:
        path = os.path.abspath(os.path.dirname(testfile))

    # Get the server from the environment variable "ALS", trying on the PATH
    # if it's not defined
    if 'ALS' in os.environ:
        als = os.environ['ALS']
    else:
        als = find_executable('ada_language_server')

    # Parse the testfile
    test = test_file_to_test(testfile)

    # Launch the als...
    p = subprocess.Popen(als, shell=True, stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE)

    # ... and feed it the test string ...
    output = p.communicate(requests_to_protocol_string(
                                test[SEND_KEY], path))[0]

    # ... and get the results as a Python list
    results = protocol_string_to_requests(output, path)

    # Compare the expected list with the actual list
    errors = []

    for i in range(max(len(results), len(test[RECEIVE_KEY]))):
        if i < len(results):
            gotten = results[i]
        else:
            gotten = "no result"

        if i < len(test[RECEIVE_KEY]):
            expected = test[RECEIVE_KEY][i]
        else:
            expected = "nothing expected"

        if gotten != expected:
            # Format the error for printing
            errors.append("---- expected ----\n{}\n---- got ----\n{}\n".format(
                            expected, gotten))

    return errors
