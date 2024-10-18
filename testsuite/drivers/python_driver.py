import importlib.util
import inspect
import os
import sys

from drivers import ALSTestDriver
from drivers.lsp_python_driver import set_debug_mode, set_wait_factor
from e3.testsuite.driver.classic import TestAbortWithFailure


class PythonTestDriver(ALSTestDriver):
    """Each test should have:
    - a test.yaml containing
          title: '<test name>'
          driver: python

    - a number of test drivers, in .json files.
    """

    def run(self):
        # The working directory
        wd = self.test_env["working_dir"]

        os.environ["ALS"] = self.env.als
        os.environ["ALS_HOME"] = self.env.als_home

        if self.env.main_options.debug:
            set_debug_mode(True)

        if self.env.wait_factor:
            set_wait_factor(self.env.wait_factor)

        # If there is a "test.py", evaluate it
        if os.path.exists(os.path.join(wd, "test.py")):
            # Spawn a separate executable for the test. The reason
            # for this is that we want to be able to remove the
            # test directory after the test is done, but we can't
            # do that under Windows if this Python driver still has
            # a handle on the module file.
            cmd = [
                sys.executable,
                __file__,
                os.path.join(wd, "test.py"),
                self.env.als,
                self.env.als_home,
                str(self.env.wait_factor),
            ]
            self.shell(
                cmd,
                cwd=wd,
                env={
                    "PYTHONPATH": os.path.dirname(os.path.dirname(__file__)),
                },
                ignore_environ=False,
            )
        else:
            raise TestAbortWithFailure("No test.py found in %s" % wd)


def run_a_module(
    test_py_path: str, als: str, als_home: str, wait_factor: float, debug: bool
):
    if debug:
        set_debug_mode(True)

    set_wait_factor(wait_factor)

    wd = os.path.dirname(test_py_path)

    # Load test.py as a module
    spec = importlib.util.spec_from_file_location("module.name", test_py_path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)

    # Look for functions with the decorator @simple_test and run them
    errors = [f"no function with @simple_test or @complex_test found in {test_py_path}"]

    for _, obj in inspect.getmembers(module):
        if inspect.isfunction(obj) and (
            hasattr(obj, "simple_test") or hasattr(obj, "complex_test")
        ):
            errors = obj(wd)

    if len(errors) > 0:
        print("\n".join(errors))
        sys.exit(1)
    sys.exit(0)


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Run a Python test driver")
    parser.add_argument("test", help="The test to load")
    parser.add_argument("als", help="The ALS program to run")
    parser.add_argument("als_home", help="The ALS home directory")
    parser.add_argument("wait_factor", type=float, help="The wait factor")
    parser.add_argument("--debug", action="store_true", help="Enable debug mode")
    args = parser.parse_args()
    run_a_module(args.test, args.als, args.als_home, args.wait_factor, args.debug)
