import glob
import os
import sys

from e3.os.process import Run, PIPE
from e3.testsuite.result import TestStatus

from drivers import ALSTestDriver

class JsonTestDriver(ALSTestDriver):
    """ Each test should have:
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
            # Put this directory in the PYTHONPATH
            os.environ["PYTHONPATH"] = os.path.dirname(__file__) + os.pathsep + os.environ.get("PYTHONPATH", "")

            # Launch the test.py script
            process = Run(
                ["python", "test.py"],
                cwd=wd,
                env={
                    "ALS": self.env.als,
                    "ALS_HOME": self.env.als_home,
                    "ALS_WAIT_FACTOR": str(self.env.wait_factor),
                },
                ignore_environ=False,
                error=sys.stderr,
                output=PIPE,
            )
            if process.status:
                # Nonzero status?
                status = TestStatus.FAIL

            output = process.out
            # If there is an output, that's a failure
            if output:
                status = TestStatus.FAIL
                if self.result.out is None:
                    self.result.out = output
                else:
                    self.result.out += output

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
