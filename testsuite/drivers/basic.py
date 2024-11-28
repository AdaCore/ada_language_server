import glob
import logging
import os
import sys

from e3.os.process import Run, command_line_image


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

            env = {
                "ALS": self.env.als,
                "ALS_HOME": self.env.als_home,
                "ALS_WAIT_FACTOR": str(self.env.wait_factor),
                "PYTHON": sys.executable,
            }

            if self.env.main_options.debug:
                cmd.append("--debug")

                # In debug mode, we want to run the subprocess without piping so that it
                # can communicate the PID to debug and wait for input. So we use
                # e3.process.Run directly instead of self.shell.
                logging.info(f"Running: cd {wd}; {command_line_image(cmd)}")
                p = Run(
                    cmd,
                    cwd=wd,
                    env=env,
                    ignore_environ=False,
                    output=None,
                )
                if p.status != 0:
                    raise TestAbortWithFailure("non-zero status code")
            else:
                p = self.shell(
                    cmd,
                    cwd=wd,
                    env=env,
                    ignore_environ=False,
                )

                if self.env.main_options.verbose:
                    print(p.out)
