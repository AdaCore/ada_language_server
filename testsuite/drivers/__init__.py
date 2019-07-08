import logging
import os
import traceback

from distutils.spawn import find_executable

from e3.fs import sync_tree
from e3.os.fs import df
from e3.testsuite.driver import TestDriver
from e3.testsuite.result import TestStatus


TESTSUITE_ROOT_DIR = os.path.dirname(
    os.path.dirname(os.path.abspath(__file__)))

PACKAGE_ROOT_DIR = os.path.dirname(TESTSUITE_ROOT_DIR)


class ALSTestDriver(TestDriver):
    """Abstract class to share some common facilities."""

    @property
    def repo_base(self):
        """
        Root directory for the "ada_language_server" repository.
        """
        return os.path.abspath(os.path.join(os.path.dirname(__file__),
                                            '..', '..'))

    def lookup_program(self, *args):
        """
        If os.path.join(self.repo_base, *args) is the location of a valid file,
        return it.  Otherwise, return the result of `find_executable` for its
        base name.
        """
        path = os.path.join(self.repo_base, *args)
        if os.path.isfile(path):
            return path
        return find_executable(os.path.basename(path))

    def add_test(self, dag):
        """
        Default workflow for testcases.

        Subclasses are free to override this if they need anything more
        complicated.
        """
        self.add_fragment(dag, 'prepare')
        self.add_fragment(dag, 'run', after=['prepare'])

    def prepare(self, previous_values):
        """
        Create the working directory to be a copy of the test directory.
        """
        sync_tree(self.test_env['test_dir'],
                  self.test_env['working_dir'])

    def should_skip(self):
        """Handle of 'skip' in test.yaml.

        If the test should be skipped according to test.yaml, push the result
        and return True. Otherwise, just return false.
        """
        status = None
        if 'skip' in self.test_env:
            eval_env = {
                'env': self.env,
                'test_env': self.test_env,
                'disk_space': lambda: df(self.env.working_dir)}

            for candidate_status, expr in self.test_env['skip']:
                try:
                    if eval(expr, eval_env):
                        status = TestStatus[candidate_status]
                        break
                except Exception:
                    logging.error(traceback.format_exc())
                    status = TestStatus.ERROR
                    break

        if status is None:
            return False
        else:
            self.result.set_status(status)
            self.push_result()
            return True
