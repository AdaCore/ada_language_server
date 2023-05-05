import os.path

from e3.testsuite.result import TestStatus

from drivers import ALSTestDriver


class ShellTestDriver(ALSTestDriver):
    """
    Run the test.sh test program.

    """

    def run(self, previous_values, slot):
        # Check whether the test should be skipped
        if self.should_skip():
            return False

        index = os.path.abspath(os.path.join(self.test_env["test_dir"], "index.txt"))
        test_sh = os.path.join(self.env.repo_base, 'testsuite', 'shell', 'test.sh')
        p = self.run_and_log([test_sh, self.env.repo_base], cwd=self.env.working_dir)
        self.result.out += p.out

        self.result.set_status(TestStatus.PASS if p.status == 0 else TestStatus.FAIL)
        self.push_result()
