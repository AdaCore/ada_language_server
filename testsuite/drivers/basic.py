import glob
import os
import sys

from e3.os.process import Run, PIPE
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

        output = ""

        status = TestStatus.PASS

        # If there is a "test.py", evaluate it
        if os.path.exists(os.path.join(wd, "test.py")):
            # Load test.py as a module
            python_file = os.path.join(wd, "test.py")
            spec = importlib.util.spec_from_file_location("module.name", python_file)
            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)

            # Look for functions with the decorator @simple_test and run them
            errors = [f"no function with @simple_test found in {python_file}"]

            for _, obj in inspect.getmembers(module):
                if inspect.isfunction(obj) and hasattr(obj, "simple_test"):
                    errors = obj(wd)

            if len(errors) > 0:
                self.result.out = "\n".join(errors)
                status = TestStatus.FAIL

            self.result.set_status(status)
            self.push_result()

            # Stop processing, do not look for .json files
            return

        for json in glob.glob(os.path.join(wd, "*.json")):
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
