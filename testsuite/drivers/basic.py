import glob
import os

from e3.testsuite.result import TestStatus

from drivers import ALSTestDriver

import inspect
import importlib.util

from drivers.lsp_python_driver import run_simple_test


class JsonTestDriver(ALSTestDriver):
    """Each test should have:
    - a test.yaml containing
          title: '<test name>'

    - a number of test drivers, in .json files.
    """

    def run(self, previous_values, slot):
        # Check whether the test should be skipped
        if self.should_skip():
            return False

        # The working directory
        wd = self.test_env["working_dir"]

        status = TestStatus.PASS

        # Safety check, make sure we're not inavertently using the wrong
        # driver.
        json_files = glob.glob(os.path.join(wd, "*.json"))
        if not json_files:
            self.result.out = "No JSON files found in %s" % wd
            self.result.set_status(TestStatus.FAIL)
            self.push_result()
            return

        for json in json_files:
            cmd = [self.env.tester_run, json]
            if self.env.options.format:
                cmd.append("--format=%s" % self.env.options.format)

            if self.env.main_options.debug:
                cmd.append("--debug")

            process = self.run_and_log(
                cmd,
                cwd=wd,
                env={
                    "ALS": self.env.als,
                    "ALS_HOME": self.env.als_home,
                    "ALS_WAIT_FACTOR": str(self.env.wait_factor),
                    "PYTHON": sys.executable
                },
                ignore_environ=False,
            )

            if process.status:
                # Nonzero status?
                status = TestStatus.FAIL
                break

        self.result.set_status(status)
        self.push_result()
