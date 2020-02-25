#!/usr/bin/env python3
""" This script processes a traces file: it extracts the requests that seem
    to be consuming memory.

    ALS.IN and ALS.OUT need to be active for this to work.

    Usage:

        digest_traces.py <traces_file>
"""

import json
import os
import re
import sys

TRACES_LINE = \
  ' *\\[ALS\\.(IN|OUT)\\] ({.*}) \\[Watermark:. (\\d+)/ (\\d+)\\].*'
# Regexp to parse one line of traces


class Digester(object):
    """ The class that takes care of the file parsing and counting """

    def __init__(self, filename):
        """Initialize. filename is the path to the traces file"""

        self.in_re = re.compile(TRACES_LINE)

        self.last_method = "(elaboration)"
        self.last_memory_usage = 0
        # the last method and last memory usage received

        self.methods_to_memory = {}
        # cumulative memory increase indexed by method id

        self.methods_exception_count = {self.last_method: 0}
        # count the exception occurrences for each exception

        self.currently_looking_at_exception = False

        self.methods_count = {self.last_method: 1}
        # methods count indexed by method id

        # read the file
        with open(filename, "r") as f:
            for line in f.readlines():
                self.read_line(line.strip())

    def read_line(self, line):
        """Read and process one line of traces"""
        if (not self.currently_looking_at_exception and
           line.startswith("_ALS.MAIN")):
            self.currently_looking_at_exception = True
            self.methods_exception_count[self.last_method] += 1

        m = self.in_re.match(line)

        if not m:
            return

        self.currently_looking_at_exception = False

        memory = int(m.group(3))
        memory_increase = memory - self.last_memory_usage
        self.last_memory_usage = memory

        if self.last_method in self.methods_to_memory:
            self.methods_to_memory[self.last_method] += memory_increase
        else:
            self.methods_to_memory[self.last_method] = memory_increase

        # If this trace indicates a method, switch to counting that method
        d = json.loads(m.group(2))
        if "method" in d:
            self.last_method = d["method"]
            if self.last_method in self.methods_count:
                self.methods_count[self.last_method] += 1
            else:
                self.methods_count[self.last_method] = 1
            if self.last_method not in self.methods_exception_count:
                self.methods_exception_count[self.last_method] = 0

    def print_report(self):
        """Print the report to the standard output"""

        memory_and_method = [(self.methods_to_memory[k], k)
                             for k in self.methods_to_memory]
        memory_and_method.sort()
        print("\nMethod                               "
              "Calls Exceptions   Cumulative mem used\n")
        total = 0
        for j in memory_and_method:
            total += j[0]
            print("{}:{}{}\t{}\t{}".format(j[1],
                                           " " * max(1, (40 - len(j[1]))),
                                           self.methods_count[j[1]],
                                           self.methods_exception_count[j[1]],
                                           j[0]))
        print("\nTOTAL:  {}\n".format(total))


def main():
    if len(sys.argv) != 2:
        print (__doc__)

    Digester(sys.argv[1]).print_report()

if __name__ == "__main__":
    main()
