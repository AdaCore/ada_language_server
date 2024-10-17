import os

from e3.testsuite.result import TestStatus

from drivers import ALSTestDriver

import inspect
import importlib.util

from drivers.lsp_python_driver import set_debug_mode, set_wait_factor


class PythonTestDriver(ALSTestDriver):
    """Each test should have:
    - a test.yaml containing
          title: '<test name>'
          driver: python

    - a number of test drivers, in .json files.
    """

    def run(self, previous_values, slot):
        # Check whether the test should be skipped
        if self.should_skip():
            return False

        # The working directory
        wd = self.test_env["working_dir"]

        status = TestStatus.PASS

        os.environ["ALS"] = self.env.als
        os.environ["ALS_HOME"] = self.env.als_home

        if self.env.main_options.debug:
            set_debug_mode(True)

        if self.env.wait_factor:
            set_wait_factor(self.env.wait_factor)


        # If there is a "test.py", evaluate it
        if os.path.exists(os.path.join(wd, "test.py")):
            # Load test.py as a module
            python_file = os.path.join(wd, "test.py")
            spec = importlib.util.spec_from_file_location("module.name", python_file)
            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)

            # Look for functions with the decorator @simple_test and run them
            errors = [
                f"no function with @simple_test or @complex_test found in {python_file}"
            ]

            for _, obj in inspect.getmembers(module):
                if inspect.isfunction(obj) and (
                    hasattr(obj, "simple_test") or hasattr(obj, "complex_test")
                ):
                    errors = obj(wd)

            if len(errors) > 0:
                self.result.log = "\n".join(errors)
                status = TestStatus.FAIL

            self.result.set_status(status)
            self.push_result()
