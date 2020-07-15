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


def traces_to_test(inout_file, project_root=None, input_only=False):
    """Take as parameter the name of a file that was generated using the
       i/o traces of ada_language_server, and the root directory of the
       project that the ada_language_server was run on, and return
       a list for it - this list is "as is" the json format in use
       for our tests.

       inout_file: path to a file which was created via the ALS.IN
         and ALS.OUT traces
       project_root: the full path to where the project sits.
          if project_root is None, do not replace paths.

       input_only: whether to consider only the input

       return: a list representing the test
    """

    groups = []
    # A list of groups. Each group is a dict with "sends" containing the
    # request to send, and "waits" containing a list of things to await
    # corresponding to that request
    with open(inout_file, 'rb') as f:
        in_re = re.compile(b' *([^{]*) ({.*})')
        for line in f.readlines():
            m = in_re.match(line)
            if m:
                is_input = m.group(1) == b"[ALS.IN]"
                cleaned = m.group(2)
                if project_root:
                    cleaned = re.sub('"file://?{}/?([^"]*)"'.format(project_root),
                                     '"$URI{\\1}"', cleaned.decode('ascii')
                                     ).replace('URI{}', 'URI{.}')
                d = json.loads(cleaned)

                if is_input:
                    # Found an "in" line. Create a group and append it
                    groups.append({"sends": d, "waits": []})
                else:
                    if not input_only:
                        # Found an "out" line: add it to the last bit of "in"
                        del(d['jsonrpc'])  # btw we remove jsonrpc from waits
                        groups[-1]["waits"].append(d)

    # Generate the test header
    result = [{"comment": ["test automatically generated"]},
              {"start": {"cmd": ["${ALS}"]}}]

    # Generate all the send commands
    for g in groups:
        result.append({"send": {"request": g["sends"], "wait": g["waits"]}})

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
