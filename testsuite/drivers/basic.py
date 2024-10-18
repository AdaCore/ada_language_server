import glob
import os
import sys

from drivers import ALSTestDriver
from e3.testsuite.driver.classic import TestAbortWithFailure


class JsonTestDriver(ALSTestDriver):
    """Each test should have:
    - a test.yaml containing
          title: '<test name>'

    - a number of test drivers, in .json files.
    """

    def run(self):
        # The working directory
        wd = self.test_env["working_dir"]

        # Safety check, make sure we're not inavertently using the wrong
        # driver.
        json_files = glob.glob(os.path.join(wd, "*.json"))
        if not json_files:
            raise TestAbortWithFailure("No JSON files found in %s" % wd)

        for json in json_files:
            cmd = [self.env.tester_run, json]
            if self.env.options.format:
                cmd.append("--format=%s" % self.env.options.format)

            if self.env.main_options.debug:
                cmd.append("--debug")

            self.shell(
                cmd,
                cwd=wd,
                env={
                    "ALS": self.env.als,
                    "ALS_HOME": self.env.als_home,
                    "ALS_WAIT_FACTOR": str(self.env.wait_factor),
                    "PYTHON": sys.executable,
                },
                ignore_environ=False,
            )
