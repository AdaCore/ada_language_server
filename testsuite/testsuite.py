#!/usr/bin/env python

import datetime
import logging
import os

from distutils.spawn import find_executable
from e3.testsuite import Testsuite

from drivers.basic import JsonTestDriver
from drivers.codecs import CodecsTestDriver
from drivers.gnatcov import GNATcov

VALGRIND_OPTIONS = [
        "--quiet",                   # only print errors
        "--tool=memcheck",           # the standard tool
        "--leak-check=full",         # report memory leaks
        # "--num-callers=100",       # more frames in call stacks
        # "--gen-suppressions=all",  # use this to generate suppression entries
        "--suppressions={base}/testsuite/leaks.supp", # the suppressions file
        ]


class ALSTestsuite(Testsuite):
    DRIVERS = {'default': JsonTestDriver,
               'codecs': CodecsTestDriver}

    # We don't have a "tests" directory but on the other hand we don't want to
    # consider every directory. So start with the whole testsuite directory,
    # and then discard specific items we find there.
    TEST_SUBDIR = '.'
    TEST_BLACKLIST = {'drivers', 'out', 'spawn'}

    def add_options(self):
        self.main.argument_parser.add_argument(
            "--build",
            default="",
            action="store",
            help="Ignored, here for compatibility purposes")
        self.main.argument_parser.add_argument(
            "--gnatcov",
            help="If provided, compute the source code coverage of testcases"
                 " on ALS. This requires GNATcoverage working with"
                 " instrumentation. The argument passed must be a directory"
                 " that contains all SID files.")
        self.main.argument_parser.add_argument(
            "--valgrind_memcheck", action="store_true",
            help="Runs the Ada Language Server under valgrind, in memory"
                 " check mode. This requires valgrind on the PATH.")

    def lookup_program(self, *args):
        """
        If os.path.join(self.repo_base, '.obj' ,*args) is the location of a
        valid file, return it.  Otherwise, return the result of
        `find_executable` for its base name.
        """
        # If the given location points to an existing file, return it
        path = os.path.join(self.env.repo_base, '.obj', *args)
        if os.path.isfile(path):
            return path

        # Otherwise, look for the requested program name in the PATH.
        #
        # TODO (S710-005): for some reason, on Windows we need to strip the
        # ".exe" suffix for the tester-run program to be able to spawn ALS.
        result = find_executable(os.path.basename(path))
        if result is None:
            raise RuntimeError('Could not find executable for {}'
                               .format(os.path.basename(path)))
        return (result[:-len('.exe')]
                if result.endswith('.exe') else
                result)

    def tear_up(self):
        # Root directory for the "ada_language_server" repository
        self.env.repo_base = os.path.abspath(os.path.join(
            os.path.dirname(__file__), '..'))

        self.env.wait_factor = 1

        # Absolute paths to programs that test drivers can use
        if self.env.options.valgrind_memcheck:
            self.env.als = "{} {} {}".format(
                self.lookup_program("valgrind"),
                " ".join(VALGRIND_OPTIONS).format(base=self.env.repo_base),
                self.lookup_program('server', 'ada_language_server'))
            self.env.wait_factor = 20  # valgrind is slow
        else:
            self.env.als = self.lookup_program('server', 'ada_language_server')
        self.env.tester_run = self.lookup_program('tester', 'tester-run')
        self.env.codec_test = self.lookup_program('codec_test', 'codec_test')

        # If code coverage is requested, initialize our helper and build
        # instrumented programs.
        self.env.gnatcov = (GNATcov(self)
                            if self.env.options.gnatcov else
                            None)

        self.start_time = datetime.datetime.now()

    def tear_down(self):
        self.stop_time = datetime.datetime.now()
        elapsed = self.stop_time - self.start_time
        logging.info('Elapsed time: {}'.format(elapsed))

        if self.env.gnatcov:
            self.env.gnatcov.report()

        super(ALSTestsuite, self).tear_down()

    def get_test_list(self, sublist):
        results = []

        blacklist = {os.path.abspath(os.path.join(self.test_dir, item))
                     for item in self.TEST_BLACKLIST}
        ada_lsp_dir = os.path.abspath(os.path.join(self.test_dir, 'ada_lsp'))

        for dirpath, dirnames, filenames in os.walk(self.test_dir):
            dirpath = os.path.abspath(dirpath)

            # Ignore paths in the blacklist
            if any(dirpath.startswith(item) for item in blacklist):
                continue

            # Warn about ada_lsp sub-directories that have no test.yaml file
            if 'test.yaml' in filenames:
                results.append(os.path.relpath(
                    os.path.join(dirpath, 'test.yaml'),
                    self.test_dir))

            elif os.path.dirname(dirpath) == ada_lsp_dir:
                print ("No test.yaml in {}".format(dirpath))
                logging.warn('No test.yaml in %s', dirpath)

        # If requested, keep only testcases that match one sublist item
        if sublist:
            logging.info('Filtering tests: %s', sublist)
            results = [t for t in results
                       if any(s in t for s in sublist)]

        logging.info('Found %s tests', len(results))
        logging.debug('tests:%s', '\n'.join('  ' + r for r in results))
        return results

    @property
    def default_driver(self):
        return 'default'
