from drivers import ALSTestDriver
from e3.testsuite.process import check_call


class ShellTestDriver(ALSTestDriver):
    """
    Run the test.sh test program.

    """

    def run(self):
        # This takes care of failing the test in case the return code is
        # non-zero
        check_call(
            self, [self.working_dir("test.sh"), self.env.repo_base], parse_shebang=True
        )
