#!/usr/bin/env python

import logging
import os

from distutils.spawn import find_executable
from e3.testsuite import Testsuite

from drivers.basic import JsonTestDriver
from drivers.codecs import CodecsTestDriver
from drivers.gnatcov import GNATcov


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
            "--gnatcov", action="store_true",
            help="Compute the source code coverage of testcases on ALS. This"
                 " requires GNATcoverage working with instrumentation and will"
                 " run a build of ALS before running tests.")

    def lookup_program(self, *args):
        """
        If os.path.join(self.repo_base, '.obj' ,*args) is the location of a
        valid file, return it.  Otherwise, return the result of
        `find_executable` for its base name.
        """
        path = os.path.join(self.env.repo_base, '.obj', *args)
        if os.path.isfile(path):
            return path
        return find_executable(os.path.basename(path))

    def tear_up(self):
        # Root directory for the "ada_language_server" repository
        self.env.repo_base = os.path.abspath(os.path.join(
            os.path.dirname(__file__), '..'))

        # Absolute paths to programs that test drivers can use
        self.env.als = self.lookup_program('server', 'ada_language_server')
        self.env.tester_run = self.lookup_program('tester', 'tester-run')
        self.env.codec_test = self.lookup_program('codec_test', 'codec_test')

        # If code coverage is requested, initialize our helper and build
        # instrumented programs.
        if self.env.options.gnatcov:
            self.env.gnatcov = GNATcov(self)
            self.env.gnatcov.build(self.env.options.jobs)
        else:
            self.env.gnatcov = None

    def tear_down(self):
        super(ALSTestsuite, self).tear_down()

        if self.env.gnatcov:
            self.env.gnatcov.report()

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
                logging.warn('No test.yaml in %s', dirpath)

        logging.info('Found %s tests', len(results))
        logging.debug('tests:%s', '\n'.join('  ' + r for r in results))
        return results

    @property
    def default_driver(self):
        return 'default'
