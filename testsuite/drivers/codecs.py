import os.path

from e3.testsuite.result import TestStatus

from drivers import ALSTestDriver


class CodecsTestDriver(ALSTestDriver):
    """
    Run the "codec_test" test program.

    Each test must have an "index.txt" file and the corresponding "*.json"
    files.
    """

    def run(self, previous_values, slot):
        # Check whether the test should be skipped
        if self.should_skip():
            return False

        index = os.path.abspath(os.path.join(self.test_env["test_dir"], "index.txt"))
        p = self.run_and_log([self.env.codec_test], cwd=self.env.repo_base, input=index)
        self.result.out += p.out

        self.result.set_status(TestStatus.PASS if p.status == 0 else TestStatus.FAIL)
        self.push_result()
