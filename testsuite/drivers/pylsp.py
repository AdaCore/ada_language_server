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
import urllib
import urllib.parse
from pathlib import Path
from typing import Any, Awaitable, Callable, Sequence

import lsprotocol.converters
import lsprotocol.types
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
    MessageType,
    Position,
    Range,
    SymbolKind,
    TextDocumentIdentifier,
    TextDocumentItem,
    WorkDoneProgressEnd,
)

# Expose aliases of these pytest-lsp classes
LanguageClient = pytest_lsp.LanguageClient

LOG = logging.getLogger(__name__)


class PyLSP(ALSTestDriver):
    """This is test driver leverages the pytest-lsp API to provide a LanguageClient
    object able to send and receive messages to the ALS through a typed API.

    The test is implemented in a test.py file where functions implementing tests have to
    be annotated with @simple_test().

    The test is run by spawning a dedicated Python subprocess in order to avoid
    lingering file handles after a test is done.
    """

    def run(self) -> None:
        wd = Path(self.working_dir())

        # If there is a "test.py", evaluate it
        if (wd / "test.py").exists():
            # Load test.py as a module
            python_file = wd / "test.py"

            cmd = [
                sys.executable,
                # Running the test is handled by this same file pylsp.py, invoked as a
                # main entry point
                __file__,
                str(python_file),
            ]

            for _ in range(0, self.env.main_options.verbose):  # type: ignore
                # Transfer -v arguments to the Python test driver to reflect verbosity
                cmd += ["-v"]

            # Spawn a child Python process to run the test, to avoid lingering file
            # handles which can create issues on Windows when the test work dir is
            # cleaned up after execution.
            p: ProcessResult = self.shell(
                cmd,
                # Start the test process directly in the test work dir. This way tests
                # can simply assume that the current dir is the test work dir.
                cwd=str(wd),
                env={
                    "ALS": self.env.als,
                    "ALS_HOME": self.env.als_home,
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
    """Log all log messages received through LSP onto the Python 'logging' module."""
    LOG.info("### LSP Log Message ###\n%s", to_str(client.log_messages))


def log_lsp_diagnostics(client: LanguageClient):
    """Log all diagnostics received through LSP."""
    LOG.info("### Diagnostics ###\n%s", to_str(client.diagnostics))


def to_str(lsp_value: Any) -> str:
    """Serialize a typed object from the lsprotocol API (e.g. list[Diagnostic]) into a
    JSON string."""
    return json.dumps(
        lsprotocol.converters.get_converter().unstructure(lsp_value), indent=2
    )


# Recording is active by default, and can be deactivated with ALS_TEST_RECORD=0. This
# could be useful in production where recording replay files is not needed.
_record_messages = os.environ.get("ALS_TEST_RECORD", "1") != "0"
_replay_devtools = Path("replay-devtools.txt")
_replay = Path("replay.txt")


def process_replay(in_path: Path, out_path: Path) -> None:
    """lsp-devtools records the JSON part of messages, but not the lower-layer message
    headers. Replaying directly with ALS requires the headers, so this function
    processes the replay file to re-write it with headers.
    """
    with in_path.open() as in_fp:
        with out_path.open("wb") as out_fp:
            for line in in_fp:
                line = line.strip()
                out_fp.write(
                    f"Content-Length:{len(line)}\r\n\r\n{line}".encode("utf-8")
                )


async def start_lsp_client(
    config: pytest_lsp.ClientServerConfig,
) -> tuple[LanguageClient, asyncio.subprocess.Process | None]:
    """Start ALS and connect it to a LanguageClient object that provides all
    communication with the ALS through a typed API exposing all LSP requests and
    notifications.

    If message recording is enabled, this function also starts a lsp-devtools process
    which intercepts exchanges between the client and the server and saves them to a
    file.

    Both the LSP client and the lsp-devtools process are returned because it is the
    responsibility of the caller to terminate both of them in a 'finally' block.
    """
    devtools = None
    if _record_messages and not args.devtools_external:
        LOG.info("Starting lsp-devtools to record messages")
        LOG.info("The JSON replay file is: %s", _replay_devtools.resolve())
        LOG.info("The replay file for debugging is: %s", _replay.resolve())
        # Start an instance of lsp-devtools to record the exchanges
        devtools = await asyncio.create_subprocess_exec(
            "lsp-devtools",
            "record",
            "--port",
            str(args.devtools_port),
            "--to-file",
            str(_replay_devtools),
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
        devtools=str(args.devtools_port) if _record_messages else None,
    )  # type: ignore

    return client, devtools


ALS_USE_LSP_DEVTOOLS_WRAPPER = (
    os.environ.get("ALS_USE_LSP_DEVTOOLS_WRAPPER", "1") != "0"
)


class ALSClientServerConfig(pytest_lsp.ClientServerConfig):

    def _get_devtools_command(self, server: str) -> list[str]:
        """We override this method to use our own lsp-devtools entry point for the
        recording of messages in a replay file.

        The purpose of our wrapper is to catch the CancelledError exception which occurs
        systematically in the nominal termination of the lsp-devtools agent. These
        exceptions appear even in successful tests and cause confusion, hence using this
        wrapper to mask them.
        """
        cmd = super()._get_devtools_command(server)

        if ALS_USE_LSP_DEVTOOLS_WRAPPER:
            # Remove the original lsp-devtools entry point
            cmd.pop(0)

            # Replace it with our own wrapper
            wrapper = Path(__file__).parent / "lsp-devtools-wrapper.py"
            cmd = [sys.executable, str(wrapper)] + cmd

        return cmd


WRAPPER_ATTRIBUTE = "__wrapper"


def test(
    config: ALSClientServerConfig | None = None,
    initialize: bool = True,
    shutdown: bool = True,
    assert_no_lsp_errors: bool = True,
) -> Callable:
    """A decorator to mark a function as a test entry point. The function must receive a
    single parameter of type LanguageClient.

    The decorator creates the LanguageClient object and passes it to the test function.

    `config` can be specified to overwrite the default configuration which gets the ALS
    executable from the `ALS` env variable and calls it with no arguments.

    If `initialize=True` (the default), the decorator takes care of sending the
    'initialize' and 'initialized' messages such that the test function can focus only
    on the testing concern.

    If `shutdown=True` (the default), the decorator also takes care of sending
    'shutdown' and 'exit' messages at the end of a test.

    `initialize=False` and/or `shutdown=False` may be used to write advanced tests that
    need control over when these requests get sent.

    :param config: a custom configuration object allowing to customize the ALS command
    line or its environment.
    :param initialize: whether the LSP initialization sequence should be performed
    before calling the test function.
    :param shutdown: whether the LSP shutdown sequence should be performed after the end
    of the test function.
    :param assert_no_lsp_errors: automatically assert that no LSP log message of level
    error were received after the end of the test function.
    """

    async def async_wrapper(
        func: Callable[[LanguageClient], Awaitable[object]]
    ) -> None:
        als = os.environ.get("ALS", "ada_language_server")
        command = [als]
        conf = config or ALSClientServerConfig(command)
        client: LanguageClient | None = None

        devtools = None
        try:
            client, devtools = await start_lsp_client(conf)
            assert client

            if initialize:
                await client.initialize_session(
                    params=InitializeParams(
                        ClientCapabilities(),
                        # ALS doesn't support the newer workspaceFolders property so we
                        # have to use the older rootURI property.
                        root_uri=URI(os.getcwd()),
                    )
                )

            # Run the test
            await func(client)

            if assert_no_lsp_errors:
                # Assert the absence of Error LSP log messages
                assertNoLSPErrors(client)
        finally:
            try:
                if client:
                    log_lsp_logs(client)
                    log_lsp_diagnostics(client)
                    if shutdown:
                        await client.shutdown_session()
            finally:
                if devtools:
                    try:
                        devtools.send_signal(signal.SIGINT)
                        # If we need these outputs in the future, here is how we get
                        # them:
                        # stdout, stderr = await devtools.communicate()
                        status = await devtools.wait()
                        LOG.info(
                            "'lsp-devtools record' exit code: %d",
                            # "\nout:\n%s\nerr:\n%s",
                            status,
                            # stdout,
                            # stderr,
                        )
                    except ProcessLookupError:
                        # The devtools process already ended because it detected the
                        # shutdown procedure
                        pass
                    finally:
                        if _replay_devtools.exists():
                            process_replay(_replay_devtools, _replay)

    def wrapper(func: Callable[[LanguageClient], Awaitable[object]]):
        def inner_wrapper():
            asyncio.run(async_wrapper(func))

        setattr(inner_wrapper, WRAPPER_ATTRIBUTE, test)
        return inner_wrapper

    return wrapper


def run_test_file(test_py_path: str):
    """This function loads the given Python file and iterates over functions declared
    with the 'simple_test' decorator and runs them.
    """
    # Load test.py as a module
    LOG.debug("Loading test file: %s", test_py_path)
    spec = importlib.util.spec_from_file_location("module.name", test_py_path)
    assert spec
    module = importlib.util.module_from_spec(spec)
    assert spec.loader
    spec.loader.exec_module(module)

    # Look for functions with the decorator @simple_test and run them
    test_functions = [
        obj
        for _, obj in inspect.getmembers(module)
        if inspect.isfunction(obj) and getattr(obj, WRAPPER_ATTRIBUTE, None) == test
    ]
    if test_functions:
        for obj in test_functions:
            obj()
    else:
        LOG.critical(
            f"No functions with @{__name__}.{test.__name__}() decorator found in {test_py_path}"
        )
        sys.exit(1)


class CLIArgs(argparse.Namespace):
    """A class to represent CLI arguments of this file when it is ran as a main entry
    point.
    """

    verbose: int = 0
    devtools_port: int = 8765
    devtools_external: bool = False
    debug: bool = False


args = CLIArgs()


def main():
    p = argparse.ArgumentParser()
    p.add_argument("test_py_path")
    p.add_argument(
        "--verbose",
        "-v",
        action="count",
        default=CLIArgs.verbose,
        help="Increase verbosity of console logging (can be given multiple times).",
    )
    p.add_argument(
        "--devtools-port",
        type=int,
        default=CLIArgs.devtools_port,
        help="Specify the port to use for lsp-devtools (default: %(default)s)",
    )
    p.add_argument(
        "--devtools-external",
        action="store_true",
        help="Don't spawn lsp-devtools internally for message saving. Instead,"
        " connect to a lsp-devtools spawned externally by the developer"
        " for inspection.",
    )

    p.parse_args(namespace=args)

    logging.basicConfig(
        level=logging.DEBUG if args.verbose > 0 else logging.INFO,
        format="%(asctime)s %(name)-20s %(levelname)-8s %(message)s",
        datefmt="%H:%M:%S",
    )

    if args.verbose < 2:
        # For low verbosity levels, inhibit the following noisy loggers
        for logger in (
            "pygls.protocol.json_rpc",
            "pygls.feature_manager",
            "pygls.client",
            "pytest_lsp.client",
            "asyncio",
        ):
            logging.getLogger(logger).setLevel(logging.ERROR)

    run_test_file(args.test_py_path)


def callHierarchyPrepareParams(
    src_uri: str, line_one_based: int, char_one_based: int
) -> CallHierarchyPrepareParams:
    """Shortcut for creating a CallHierarchyPrepareParams object."""
    return CallHierarchyPrepareParams(
        TextDocumentIdentifier(src_uri),
        Pos(line_one_based, char_one_based),
    )


def Pos(line_one_based: int, char_one_based: int):
    """Shortcut for creating a Position object with ONE-BASED locations."""
    return Position(line_one_based - 1, char_one_based - 1)


def RangeZero(line_one_based: int, char_one_based: int):
    """Shortcut for creating a Range that starts and ends at the same location given
    with ONE-BASED integers.
    """
    pos = Pos(line_one_based, char_one_based)
    return Range(pos, pos)


def callHierarchyIncomingCallsParams(
    src_uri: str, line_one_based: int, char_one_based: int
) -> CallHierarchyIncomingCallsParams:
    """Shortcut for creating a CallHierarchyIncomingCallsParams object."""
    rng = RangeZero(line_one_based, char_one_based)
    param = CallHierarchyIncomingCallsParams(
        item=CallHierarchyItem(
            name="",
            kind=SymbolKind.Function,
            uri=src_uri,
            range=rng,
            selection_range=rng,
        )
    )

    return param


def didOpenTextDocumentParams(
    src_path: Path | str,
) -> tuple[DidOpenTextDocumentParams, str]:
    """Create a DidOpenTextDocumentParams for the given file path. The URI of that file
    is also returned for convenience.
    """
    src_uri = URI(src_path)
    return (
        DidOpenTextDocumentParams(
            TextDocumentItem(src_uri, "ada", 0, Path(src_path).read_text())
        ),
        src_uri,
    )


def URI(src_path: Path | str) -> str:
    """Create a URI for the given file path."""
    src_abs = Path(src_path).resolve()
    src_uri = f"file://{urllib.parse.quote(str(src_abs))}"
    return src_uri


def assertEqual(actual: Any, expected: Any) -> None:
    """Raise an AssertionError if actual != expected."""
    if actual != expected:
        msg = f"\n### Actual ###\n{actual}\n### Expected ###\n{expected}"
        raise AssertionError(msg)


def assertLocationsList(
    actual: Sequence[CallHierarchyItem | CallHierarchyIncomingCall],
    expected: list[tuple[str, int]],
) -> None:
    """Assert the content of a list of results from a CallHierarchy or
    CallHierarchyIncomingCall request. Expected results are given as a list of
    (<basename>, <line-one-based>) tuples.
    """
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
    """Wait until the ALS finishes indexing."""
    indexing_progress = None
    while indexing_progress is None:
        await asyncio.sleep(0.2)
        LOG.info(
            "Awaiting indexing progress - lsp.progress_reports = %s",
            lsp.progress_reports,
        )
        indexing_progress = next(
            (prog for prog in lsp.progress_reports if "indexing" in str(prog)),
            None,
        )

    LOG.info("Received indexing progress token")

    last_progress = lsp.progress_reports[indexing_progress][-1]
    while not isinstance(last_progress, WorkDoneProgressEnd):
        await asyncio.sleep(0.2)
        LOG.info("Waiting for indexing end - last_progress = %s", last_progress)
        last_progress = lsp.progress_reports[indexing_progress][-1]

    LOG.info("Received indexing end message")


def assertNoLSPErrors(lsp: LanguageClient):
    """Assert that no Error-level log messages have been received by the LSP client so
    far.
    """
    errors = [m for m in lsp.log_messages if m.type == MessageType.Error]
    if errors:
        msg = "\n### Found LSP Errors ###\n" + to_str(errors)
        raise AssertionError(msg)
