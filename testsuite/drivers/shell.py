from e3.testsuite.result import TestStatus
from e3.testsuite.process import check_call

from drivers import ALSTestDriver


class ShellTestDriver(ALSTestDriver):
    """
    Run the test.sh test program.

    """

    def run(self, previous_values, slot):
        # Check whether the test should be skipped
        if self.should_skip():
            return False

        # This takes care of failing the test in case the return code is
        # non-zero
        check_call(
            self,
            [self.working_dir("test.sh"), self.env.repo_base],
            parse_shebang=True
        )

        self.result.set_status(TestStatus.PASS)
        self.push_result()
