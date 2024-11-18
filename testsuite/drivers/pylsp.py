import argparse
import asyncio
import importlib
import importlib.util
import inspect
import json
import logging
import os
import signal
import sys
from pathlib import Path
from typing import Awaitable, Callable, Sequence
import urllib.parse

import lsprotocol.converters
import lsprotocol.types
import urllib

import pytest_lsp

from drivers import ALSTestDriver
from e3.testsuite.driver.classic import ProcessResult, TestAbortWithFailure
from e3.testsuite.result import TestStatus
from lsprotocol.types import (
    CallHierarchyIncomingCall,
    CallHierarchyIncomingCallsParams,
    CallHierarchyItem,
    CallHierarchyPrepareParams,
    ClientCapabilities,
    DidOpenTextDocumentParams,
    InitializeParams,
    Position,
    Range,
    SymbolKind,
    TextDocumentIdentifier,
    TextDocumentItem,
    WorkDoneProgressEnd,
)

logger = logging.getLogger(__name__)

# Expose aliases of these pytest-lsp classes
LanguageClient = pytest_lsp.LanguageClient
ClientServerConfig = pytest_lsp.ClientServerConfig


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
    logging.info(
        "### Diagnostics ###\n%s",
        json.dumps(
            lsprotocol.converters.get_converter().unstructure(client.diagnostics),
            indent=2,
        ),
    )


# Recording is active by default, and can be deactivated with ALS_TEST_RECORD=0. This
# could be useful in production where recording replay files is not needed.
record_messages = os.environ.get("ALS_TEST_RECORD", "1") != "0"
replay_devtools = Path("replay-devtools.txt")
replay = Path("replay.txt")


def process_replay(input: Path, output: Path) -> None:
    """lsp-devtools records the JSON part of messages, but not the lower-layer message
    headers. Replaying directly with ALS requires the headers, so this function
    processes the replay file to re-write it with headers.
    """
    with input.open() as in_fp:
        with output.open("wb") as out_fp:
            for line in in_fp:
                line = line.strip()
                out_fp.write(
                    f"Content-Length:{len(line)}\r\n\r\n{line}".encode("utf-8")
                )


async def start_lsp_client(
    config: ClientServerConfig,
) -> tuple[LanguageClient, asyncio.subprocess.Process | None]:
    devtools = None
    if record_messages and not args.devtools_external:
        logging.info("Starting lsp-devtools to record messages")
        logging.info("The JSON replay file is: %s", replay_devtools.resolve())
        logging.info("The replay file for debugging is: %s", replay.resolve())
        # Start an instance of lsp-devtools to record the exchanges
        devtools = await asyncio.create_subprocess_exec(
            "lsp-devtools",
            "record",
            "--port",
            str(args.devtools_port),
            "--to-file",
            str(replay_devtools),
            # Only record messages emitted by the client because the goal of the
            # replay file is to be used directly as input to the ALS for
            # debugging
            "--message-source",
            "client",
            # I was hoping that the following CLI args would help record raw
            # messages in the replay file, but they seem to have no effect. I'm
            # including them here to record the fact that they have been
            # considered and deemed useless.
            # "--capture-raw-output",
            # "--capture-rpc-output",
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )

    client: LanguageClient = await config.start(
        devtools=str(args.devtools_port) if record_messages else None,
    )  # type: ignore

    return client, devtools


def simple_test(cmd: list[str] | None = None) -> Callable:

    async def async_wrapper(func: Callable[..., Awaitable[object]]) -> None:
        als = os.environ.get("ALS", "ada_language_server")
        command = cmd or [als]
        config = ClientServerConfig(command)
        client: LanguageClient | None = None

        devtools = None
        try:
            client, devtools = await start_lsp_client(config)
            assert client

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
            try:
                if client:
                    log_lsp_logs(client)
                    log_lsp_diagnostics(client)
                    await client.shutdown_session()
            finally:
                if devtools:
                    try:
                        devtools.send_signal(signal.SIGINT)
                        # If we need these outputs in the future, here is how we get
                        # them:
                        # stdout, stderr = await devtools.communicate()
                        status = await devtools.wait()
                        logging.info(
                            "lsp-devtools exit code: %d",
                            # "\nout:\n%s\nerr:\n%s",
                            status,
                            # stdout,
                            # stderr,
                        )
                    finally:
                        process_replay(replay_devtools, replay)

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


class CLIArgs(argparse.Namespace):
    verbose: int = 0
    devtools_port: int = 8765
    devtools_external: bool = False


args = CLIArgs()

if __name__ == "__main__":
    p = argparse.ArgumentParser()
    p.add_argument("test_py_path")
    p.add_argument("--verbose", "-v", action="count", default=CLIArgs.verbose)
    p.add_argument("--devtools-port", type=int, default=CLIArgs.devtools_port)
    p.add_argument("--devtools-external", action="store_true")

    p.parse_args(namespace=args)

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s %(name)-20s %(levelname)-8s %(message)s",
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


def callHierarchyPrepareParams(main_adb_uri, line_one_based, char_one_based):
    return CallHierarchyPrepareParams(
        TextDocumentIdentifier(main_adb_uri),
        Position(line_one_based - 1, char_one_based - 1),
    )


def callHierarchyIncomingCallsParams(main_adb_uri, line_one_based, char_one_based):
    pos = Position(line_one_based - 1, char_one_based - 1)
    range = Range(pos, pos)
    param = CallHierarchyIncomingCallsParams(
        item=CallHierarchyItem(
            name="",
            kind=SymbolKind.Function,
            uri=main_adb_uri,
            range=range,
            selection_range=range,
        )
    )

    return param


def didOpenTextDocumentParams(
    src_path: Path | str,
) -> tuple[DidOpenTextDocumentParams, str]:
    src_uri = URI(src_path)
    return (
        DidOpenTextDocumentParams(
            TextDocumentItem(src_uri, "ada", 0, Path(src_path).read_text())
        ),
        src_uri,
    )


def URI(src_path: Path | str) -> str:
    src_abs = Path(src_path).resolve()
    src_uri = f"file://{urllib.parse.quote(str(src_abs))}"
    return src_uri


def assertEqual(actual, expected) -> None:
    if actual != expected:
        msg = f"### Actual ###\n{actual}\n### Expected ###\n{expected}"
        raise AssertionError(msg)


def assertLocationsList(
    actual: Sequence[CallHierarchyItem | CallHierarchyIncomingCall],
    expected: list[tuple[str, int]],
) -> None:
    actual_locations = []
    for item in actual:
        if isinstance(item, CallHierarchyIncomingCall):
            item = item.from_

        basename = os.path.basename(item.uri)
        line = item.range.start.line + 1
        actual_locations.append((basename, line))

    def to_str(item: tuple[str, int]):
        return f"   {item[0]}:{item[1]}"

    assertEqual(
        "\n".join(map(to_str, actual_locations)),
        "\n".join(map(to_str, expected)),
    )


async def awaitIndexingEnd(lsp: LanguageClient):
    indexing_progress = None
    while indexing_progress is None:
        await asyncio.sleep(0.2)
        logging.info(
            "Awaiting indexing progress - lsp.progress_reports = %s",
            lsp.progress_reports,
        )
        indexing_progress = next(
            (prog for prog in lsp.progress_reports if "indexing" in str(prog)),
            None,
        )

    logging.info("Received indexing progress token")

    last_progress = lsp.progress_reports[indexing_progress][-1]
    while not isinstance(last_progress, WorkDoneProgressEnd):
        await asyncio.sleep(0.2)
        logging.info("Waiting for indexing end - last_progress = %s", last_progress)
        last_progress = lsp.progress_reports[indexing_progress][-1]

    logging.info("Received indexing end message")
