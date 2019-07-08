from e3.fs import mkdir, sync_tree, echo_to_file
from e3.testsuite.process import Run
from e3.testsuite.result import TestStatus
from drivers import ALSTestDriver
from distutils.spawn import find_executable
import os
import glob


class JsonTestDriver(ALSTestDriver):
    """ Each test should have:
          - a test.yaml containing
                title: '<test name>'

          - a number of test drivers, in .json files.
    """

    def run(self, previous_values):
        # Check whether the test should be skipped
        if self.should_skip():
            return False

        # The working directory
        wd = self.test_env['working_dir']

        als = self.lookup_program('.obj', 'server', 'ada_language_server')
        tester_run = self.lookup_program('.obj', 'tester', 'tester-run')

        output = ""

        status = TestStatus.PASS

        for json in glob.glob(os.path.join(wd, '*.json')):
            process = Run(
                [tester_run, json],
                cwd=wd,
                timeout=120,
                env={'ALS': als},
                ignore_environ=False)
            output += process.out

            if process.status:
                # Nonzero status?
                if process.status == 100:
                    # This one is an xfail
                    status = TestStatus.XFAIL
                    break
                else:
                    # Unknown status!
                    status = TestStatus.ERROR
                    break
            else:
                # Status is 0...
                if output:
                    # ... and there is an output: that's a FAIL
                    status = TestStatus.FAIL
                    break

        self.result.set_status(status)
        if output:
            # If there's an output, capture it
            self.result.out = output

        self.push_result()
