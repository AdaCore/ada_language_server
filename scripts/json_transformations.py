""" A set of utilities for transforming json/tests/ALS traces data for the
    purposes of easing debugging and writing tests.
"""

import re
import json
import yaml

SEND_KEY = "send"
WAIT_KEY = "wait"
# The keys in the json format for tests that represent, respectively,
# what we send and what we expect to receive.


def traces_to_test(input_file, output_file, project_root=None):
    """Take as parameter the name of a file that was generated using the
       i/o traces of ada_language_server, and the root directory of the
       project that the ada_language_server was run on, and return
       a list for it - this list is "as is" the json format in use
       for our tests.

       input_file: path to a file which was created via the ALS.IN trace
       output_file: path to a file which was created via the ALS.OUT trace
       project_root: the full path to where the project sits.
          if project_root is None, do not replace paths.

       return: a list representing the test
    """

    sends = []  # list of data to send
    waits = []  # list of data to receive
    for file, l in ((input_file, sends), (output_file, waits)):
        with open(file, 'rb') as f:
            in_re = re.compile('[^{]*({.*})')
            for line in f.readlines():
                m = in_re.match(line)
                if m:
                    cleaned = m.group(1)
                    if project_root:
                        cleaned = re.sub('"{}/?(.*)'.format(project_root),
                                         'URI{\\1}', cleaned
                                         ).replace('URI{}', 'URI{.}')
                    d = json.loads(cleaned)
                    l.append(d)

    # Generate the test header
    result = [{"comment": ["test automatically generated"]},
              {"start": {"cmd": ["${ALS}"]}}]

    # Generate all the send commands
    for s in sends:
        result.append({"send": {"request": s}})

    # For now, append all the "waits" as expected in the last request
    result[-1]["wait"] = waits

    # Generate the test footer
    result.append({"stop": {"exit_code": 0}})

    return result


def python_to_protocol_string(reqs, pwd=None):
    """Take a list of requests (each one being a dict) and return a string
       ready for consumption by the ALS.

       pwd is the path to substitute to URI{} blocks.
       If None, no substitution is performed.
    """
    result = ""
    for req in reqs:
        # Add the "jsonrpc" field if needs be
        req["jsonrpc"] = "2.0"
        s = json.dumps(req, separators=(',', ':'))
        # TODO: perform path localization in s?
        # ... and add the "Content-Length" and CRLF terminators requred by the
        # protocol
        result += 'Content-Length: {}\r\n\r\n{}'.format(len(s), s)

    return result
