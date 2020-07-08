import logging
import os
import traceback

from e3.fs import sync_tree
from e3.os.fs import df
from e3.os.process import Run
from e3.testsuite.driver import TestDriver
from e3.testsuite.result import Log, TestStatus

from drivers.gnatcov import GNATcov


TESTSUITE_ROOT_DIR = os.path.dirname(
    os.path.dirname(os.path.abspath(__file__)))

PACKAGE_ROOT_DIR = os.path.dirname(TESTSUITE_ROOT_DIR)


class ALSTestDriver(TestDriver):
    """Abstract class to share some common facilities."""

    def add_test(self, dag):
        """
        Default workflow for testcases.

        Subclasses are free to override this if they need anything more
        complicated.
        """
        self.add_fragment(dag, 'prepare')
        self.add_fragment(dag, 'run', after=['prepare'])

    def prepare(self, previous_values, slot):
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

    def run_and_log(self, cmd, **kwargs):
        """
        Wrapper around e3.os.process.Run to log processes.

        Logging the processes that are run in each testcases is very useful for
        debugging.
        """

        # If code coverage is requested, leave a chance to gnatcov to decorate
        # the execution of the subprogram in order to make it contribute to
        # code coverage.
        if self.env.gnatcov:
            kwargs = self.env.gnatcov.decorate_run(self, kwargs)

        process = Run(cmd, **kwargs)

        self.result.processes.append({
            'cmd': cmd,
            'run_args': kwargs,
            'status': process.status,
            'output': Log(process.out)})

        if self.result.out is None:
            self.result.out = process.out
        else:
            self.result.out += process.out

        return process
