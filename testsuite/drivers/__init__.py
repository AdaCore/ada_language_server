import os

from e3.os.fs import df
from e3.os.process import Run
from e3.testsuite.control import TestControlCreator, YAMLTestControlCreator
from e3.testsuite.driver.classic import ClassicTestDriver, ProcessResult
from e3.testsuite.result import Log

TESTSUITE_ROOT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

PACKAGE_ROOT_DIR = os.path.dirname(TESTSUITE_ROOT_DIR)


class ALSTestDriver(ClassicTestDriver):
    """Abstract class to share some common facilities."""

    @property
    def test_control_creator(self) -> TestControlCreator:
        eval_env = {
            "env": self.env,
            "test_env": self.test_env,
            "disk_space": lambda: df(self.env.working_dir),
        }

        return YAMLTestControlCreator(eval_env)

    def shell(self, *args, **kwargs) -> ProcessResult:
        if self.env.gnatcov:
            kwargs = self.env.gnatcov.decorate_run(self, kwargs)

        return super().shell(*args, **kwargs)

    def run_and_log(self, cmd, **kwargs):
        """
        Wrapper around e3.os.process.Run to log processes.

        Logging the processes that are run in each testcases is very useful for
        debugging.
        """

        # If code coverage is requested, leave a chance to gnatcov to decorate
        # the execution of the subprogram in order to make it contribute to
        # code coverage.
        if self.env.gnatcov:
            kwargs = self.env.gnatcov.decorate_run(self, kwargs)

        process = Run(cmd, **kwargs)

        self.result.processes.append(
            {
                "cmd": cmd,
                "run_args": kwargs,
                "status": process.status,
                "output": Log(process.out),
            }
        )

        if "Log:" in process.out:
            output, log = process.out.split("Log:")
        else:
            output = process.out
            log = ""

        cwd = kwargs.get("cwd", os.getcwd())
        self.result.log += f"Run: cd {cwd}; {process.command_line_image()}\n"
        self.result.log += f"Status code: {process.status}\n"
        self.result.log += f"Output: {output}\n"
        self.result.log += f"Log: {log}\n"

        if self.result.out is None:
            self.result.out = output
        else:
            self.result.out += output

        return process
