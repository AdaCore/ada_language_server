import argparse
import asyncio
import importlib
import importlib.util
import inspect
import json
import logging
import os
import sys
from pathlib import Path
from typing import Awaitable, Callable

from drivers import ALSTestDriver
from e3.testsuite.driver.classic import TestAbortWithFailure, ProcessResult
from e3.testsuite.result import TestStatus
from lsprotocol.types import ClientCapabilities, InitializeParams
from pytest_lsp import ClientServerConfig, LanguageClient

from drivers.lsp_types import URI


class PyLSP(ALSTestDriver):

    def run(self) -> None:
        wd = Path(self.working_dir())

        # If there is a "test.py", evaluate it
        if (wd / "test.py").exists():
            # Load test.py as a module
            python_file = wd / "test.py"

            cmd = [
                sys.executable,
                __file__,
                str(python_file),
            ]

            for _ in range(0, self.env.main_options.verbose):  # type: ignore
                # Transfer -v arguments to the Python test
                cmd += ["-v"]

            p: ProcessResult = self.shell(
                cmd,
                # Start the test process directly in the test work dir
                cwd=str(wd),
                env={
                    "ALS": self.env.als,
                    "ALS_HOME": self.env.als_home,
                    "ALS_WAIT_FACTOR": str(self.env.wait_factor),
                    "PYTHONPATH": os.path.dirname(os.path.dirname(__file__)),
                },
                ignore_environ=False,
                timeout=15,  # seconds
            )

            if self.env.main_options.verbose:  # type: ignore
                print(p.out)

        else:
            raise TestAbortWithFailure(TestStatus.ERROR, "No test.py found in test dir")


def log_lsp_logs(client: LanguageClient):
    logging.info(
        "### LSP Log Message ###\n%s", "\n".join(map(str, client.log_messages))
    )


def log_lsp_diagnostics(client: LanguageClient):
    logging.info("### Diagnostics ###\n%s", json.dumps(client.diagnostics))


def simple_test(cmd: list[str] | None = None) -> Callable:

    async def async_wrapper(func: Callable[..., Awaitable[object]]):
        als = os.environ.get("ALS", "ada_language_server")
        command = cmd or [als]
        config = ClientServerConfig(command)
        client: LanguageClient = await config.start()  # type: ignore
        assert client
        try:
            await client.initialize_session(
                params=InitializeParams(
                    ClientCapabilities(),
                    # ALS doesn't support the newer workspaceFolders property so we have
                    # to use the older rootURI property.
                    root_uri=URI(os.getcwd()),
                )
            )
            await func(client)
        finally:
            log_lsp_logs(client)
            log_lsp_diagnostics(client)
            await client.shutdown_session()

    def wrapper(func):
        def inner_wrapper():
            asyncio.run(async_wrapper(func))

        setattr(inner_wrapper, simple_test.__name__, True)
        return inner_wrapper

    setattr(wrapper, simple_test.__name__, True)
    return wrapper


def run_test_file(test_py_path: str):
    # Load test.py as a module
    logging.debug("Loading test file: %s", test_py_path)
    spec = importlib.util.spec_from_file_location("module.name", test_py_path)
    assert spec
    module = importlib.util.module_from_spec(spec)
    assert spec.loader
    spec.loader.exec_module(module)

    # Look for functions with the decorator @simple_test and run them
    test_functions = [
        obj
        for _, obj in inspect.getmembers(module)
        if inspect.isfunction(obj) and hasattr(obj, simple_test.__name__)
    ]
    if test_functions:
        for obj in test_functions:
            obj()
    else:
        logging.critical(
            "No functions with @simple_test or @complex_test found in %s", test_py_path
        )
        sys.exit(1)


def assertEqual(actual, expected) -> None:
    if actual != expected:
        msg = f"### Actual ###\n{actual}\n### Expected ###\n{expected}"
        raise AssertionError(msg)


if __name__ == "__main__":
    p = argparse.ArgumentParser()
    p.add_argument("test_py_path")
    p.add_argument("--verbose", "-v", action="count", default=0)

    args = p.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s %(levelname)-8s %(message)s",
        datefmt="%H:%M:%S",
    )

    if args.verbose < 2:
        # For low verbosity levels, inhibit the following noisy loggers
        logging.getLogger("pygls.protocol.json_rpc").setLevel(logging.ERROR)
        logging.getLogger("pygls.feature_manager").setLevel(logging.ERROR)
        logging.getLogger("pygls.client").setLevel(logging.ERROR)
        logging.getLogger("pytest_lsp.client").setLevel(logging.ERROR)
        logging.getLogger("asyncio").setLevel(logging.ERROR)

    run_test_file(args.test_py_path)
