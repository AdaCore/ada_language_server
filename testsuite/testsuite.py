#!/usr/bin/env python

import logging
import os

from e3.fs import ls
from e3.testsuite import Testsuite

from drivers.basic import JsonTestDriver


class ALSTestsuite(Testsuite):
    TEST_SUBDIR = 'ada_lsp'
    DRIVERS = {'default': JsonTestDriver}

    def add_options(self):
        self.main.argument_parser.add_argument(
            "--build",
            default="",
            action="store",
            help="Ignored, here for compatibility purposes")

    def tear_up(self):
        pass

    def tear_down(self):
        super(ALSTestsuite, self).tear_down()

    def get_test_list(self, sublist):
        # The tests are one per subdir of "ada_lsp"
        if sublist:
            dirs = [os.path.abspath(os.path.join(self.test_dir, '..', s))
                    for s in sublist]
        else:
            dirs = ls(os.path.join(self.test_dir, '*'))
        results = []
        for d in dirs:
            if os.path.isdir(d):
                # Create the test.yamls if they don't exist!
                yaml = os.path.join(d, 'test.yaml')
                basename = os.path.basename(d)

                if not os.path.exists(yaml):
                    with open(yaml, 'wb') as f:
                        logging.info("creating {} for you :-)".format(yaml))
                        f.write("title: '{}'\n".format(basename))
                results.append(os.path.join(basename, 'test.yaml'))

        logging.info('Found %s tests %s', len(results), results)
        logging.debug("tests:\n  " + "\n  ".join(results))
        return results

    @property
    def default_driver(self):
        return 'default'
