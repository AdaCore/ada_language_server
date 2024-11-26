from drivers import ALSTestDriver
from e3.testsuite.process import check_call


class ShellTestDriver(ALSTestDriver):
    """
    Run the test.sh test program.
    """

    def run(self) -> None:

        # This takes care of failing the test in case the return code is
        # non-zero
        p = check_call(
            self,
            [self.working_dir("test.sh"), self.env.repo_base],
            parse_shebang=True,
            env={
                "ALS": self.env.als,
                "ALS_HOME": self.env.als_home,
            },
            # The following makes the child process inherit the parent process's
            # environment, in addition to the above environment.
            ignore_environ=False,
            timeout=15,  # seconds
        )

        assert self.env.main_options
        if self.env.main_options.verbose:
            print(p.out)
