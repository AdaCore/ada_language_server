""" A set of utilities for transforming json data for the purposes of
    serving it to the ALS.

    This defines a mini format which is suitable for
    writing tests, and which looks like json where
    we can have single-line comments starting with //.

    (TODO: we could also use JSON-minify for this).

    The format of a test  looks like this:
    --------------

    // comments look like this
    {
      // first the list of everything we send
      "emit": [
           {"id": 0, "method": "initialize", "params": {...}},

            // ... the rest of requests that we emit

          ],

      // then the list of everything we receive
      "receive": [
         {"id": 0, "result": {...}},

         // ... the rest of the data that we expect
       ]
    }
    --------------

    For relocatability purposes, the path to the root of the actual
    file contents is substituted to "$PWD".

    The json nodes listed correspond exactly to nodes in the language
    server protocol, except that the field "jsonrpc" is not represented,
    but automatically inserted by this code.
"""

import re
import json

SEND_KEY = "emit"
RECEIVE_KEY = "receive"
# The keys in the json format for tests that represent, respectively,
# what we send and what we expect to receive. (These particular words
# were chosen so that we can simply sort the json nodes alphabetically
# and have sent data above received data)


def traces_to_test(input_file, output_file, project_root):
    """Take as parameter the name of a file that was generated using the
       i/o traces of ada_language_server, and the root directory of the
       project that the ada_language_server was run on, and return
       a json object from it.

       input_file: path to a file which was created via the ALS.IN trace
       output_file: path to a file which was created via the ALS.OUT trace
       project_root: the full path to where the project sits.

       return: a dict representing the output, with project_root substituted
               to $PWD
    """

    result = {SEND_KEY: [], RECEIVE_KEY: []}
    for file, tag in ((input_file, SEND_KEY), (output_file, RECEIVE_KEY)):
        with open(file, 'rb') as f:
            in_re = re.compile("[^{]*({.*})")
            for line in f.readlines():
                m = in_re.match(line)
                if m:
                    cleaned = m.group(1).replace(project_root, '$PWD')
                    d = json.loads(cleaned)
                    if "jsonrpc" in d:
                        del(d["jsonrpc"])
                    result[tag].append(d)

    return result


def test_file_to_test(test_file):
    """Take as parameter a test file with the new format, and returns
       from it a dictionary test.
       In this dictionary, SEND_KEY contains what we send to the ALS,
       and RECEIVE_KEY contains what we expect in return.
    """
    result = ""
    # Very basic thing to remove comments indicated with '//'
    with open(test_file, 'rb') as f:
        for line in f.readlines():
            if not line.strip().startswith("//"):
                result += line
    return json.loads(result)


def requests_to_protocol_string(reqs, pwd):
    """Take a list of requests (each one being a dict) and return a string
       ready for consumption by the ALS.

       pwd is the path to substitute to $PWD
    """
    result = ""
    for req in reqs:
        # Add the "jsonrpc" fields and perform path localization
        req["jsonrpc"] = "2.0"
        s = json.dumps(req, separators=(',', ':')).replace("$PWD", pwd)
        # ... and add the "Content-Length" and CRLF terminators requred by the
        # protocol
        result += 'Content-Length: {}\r\n\r\n{}'.format(len(s), s)

    return result


def protocol_string_to_requests(s, pwd):
    """Convert the given protocol string to a python list, replacing pwd with
       $PWD.

       return a list of responses (each one being a dict)
    """
    # Remove the "jsonrpc" fields and perform path delocalization, and
    # remove the "Content-Length" headers, plus do a mini bit of massaging
    # so this can be loaded by json.loads
    requests = json.loads("[" + re.sub("Content-Length:.*", ",", s
                                       ).replace(pwd, '$PWD')[1:] + "]")
    for r in requests:
        if "jsonrpc" in r:
            del(r["jsonrpc"])
    return requests
