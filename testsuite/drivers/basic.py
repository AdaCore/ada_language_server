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

    def add_test(self, dag):
        self.add_fragment(dag, 'prepare')
        self.add_fragment(dag, 'run', after=['prepare'])

    def prepare(self, previous_values):
        mkdir(self.test_env['working_dir'])
        sync_tree(self.test_env['test_dir'],
                  self.test_env['working_dir'])

    def run(self, previous_values):
        # Check whether the test should be skipped
        skip = self.should_skip()
        if skip is not None:
            self.result.set_status(skip)
            self.push_result()
            return False

        # The working directory
        wd = self.test_env['working_dir']

        # The base directory for the repository
        base = os.path.abspath(os.path.join(os.path.dirname(__file__),
                                            "..", ".."))

        # Where the als resides
        als = os.path.join(base, '.obj', 'server', 'ada_language_server')
        if not os.path.isfile(als):
            als = find_executable('ada_language_server').rstrip('.exe')

        # Where the test driver resides
        tester = os.path.join(base, '.obj', 'tester', 'tester-run')
        if not os.path.isfile(tester):
            tester = find_executable('tester-run')

        output = ""

        status = TestStatus.PASS

        for json in glob.glob(os.path.join(wd, '*.json')):
            process = Run(
                [tester, json],
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
