import argparse
import asyncio
import importlib
import importlib.util
import inspect
import json
import logging
import os
import psutil
import shlex
import sys
import urllib
import urllib.parse
import uuid
import warnings
from pathlib import Path
from typing import Any, Awaitable, Callable, Literal, Sequence, Type, TypedDict

import attrs
import lsprotocol.converters
import lsprotocol.types
from drivers import ALSTestDriver
from e3.os.process import Run, command_line_image
from e3.testsuite.driver.classic import ProcessResult, TestAbortWithFailure
from e3.testsuite.result import TestStatus
from lsprotocol import types
from lsprotocol.types import (
    CallHierarchyIncomingCall,
    CallHierarchyIncomingCallsParams,
    CallHierarchyItem,
    CallHierarchyPrepareParams,
    ClientCapabilities,
    DidChangeConfigurationParams,
    DidOpenTextDocumentParams,
    ExecuteCommandParams,
    InitializeParams,
    MessageType,
    Position,
    Range,
    SymbolKind,
    TextDocumentIdentifier,
    TextDocumentItem,
    WorkDoneProgressEnd,
)
from pygls.client import JsonRPCClient
from pygls.protocol import default_converter
from pytest_lsp import (
    ClientServerConfig,
    LanguageClient,
    LanguageClientProtocol,
    LspSpecificationWarning,
)

LOG = logging.getLogger(__name__)
logger = LOG


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

            pylsp_wrapper = Path(__file__).parent / "pylsp-runner.py"
            cmd = [
                sys.executable,
                # The main entry point is implemented in a different file so that all
                # Python code imports the same pylsp module and shares its state. If the
                # main entry point was implemented in this same file, then it would
                # exist in 2 copies: one as the module "__main__", and one as the module
                # "drivers.pylsp".
                str(pylsp_wrapper),
                str(python_file),
            ]

            assert self.env.main_options
            for _ in range(0, self.env.main_options.verbose):
                # Transfer -v arguments to the Python test driver to reflect verbosity
                cmd += ["-v"]

            if self.env.main_options.debug:
                cmd.append("--debug")

            # Spawn a child Python process to run the test, to avoid lingering file
            # handles which can create issues on Windows when the test work dir is
            # cleaned up after execution.
            env = {
                "ALS": self.env.als,
                "ALS_HOME": self.env.als_home,
                "ALS_WAIT_FACTOR": str(self.env.wait_factor),
                "PYTHONPATH": os.path.dirname(os.path.dirname(__file__)),
            }

            if self.env.main_options.debug or self.env.main_options.verbose:
                # In this case we use Run directly without piping options to connect the
                # subprocess directly to the parent I/O.

                LOG.info(f"Run: cd {wd}; {command_line_image(cmd)}")
                r = Run(
                    cmd,
                    cwd=str(wd),
                    env=env,
                    ignore_environ=False,
                    output=None,
                )
                if r.status != 0:
                    raise TestAbortWithFailure("non-zero status code")
            else:
                # Usually we specify a timeout here, however a test file may
                # contain multiple tests. So instead we apply a timeout to each
                # test in async_wrapper()
                p: ProcessResult = self.shell(
                    cmd,
                    # Start the test process directly in the test work dir. This way
                    # tests can simply assume that the current dir is the test work dir.
                    cwd=str(wd),
                    env=env,
                    ignore_environ=False,
                    stdin=None,
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
ALS_TEST_RECORD = os.environ.get("ALS_TEST_RECORD", "1") != "0"
_replay = Path("replay.txt")


class RecordingTransportWrapper(asyncio.Transport):
    """This class wraps an asyncio.Transport object to intercept written data and save
    it to a replay file.
    """

    def __init__(self, wrapped: asyncio.Transport, replay: Path) -> None:
        self.wrapped = wrapped
        # Do not use buffering so that data is written to the replay file immediately
        self.replay_fp = replay.open("wb", buffering=0)

    def __del__(self):
        # Close the replay file at destruction of this object
        self.replay_fp.close()
        super().__del__()

    def __getattr__(self, attr):
        """This is a classical way of wrapping some attributes, but not others."""
        if attr in self.__dict__:
            # If the requested attribute is overriden in this object, use it
            return getattr(self, attr)
        else:
            # Otherwise use the attribute of the wrapped object
            return getattr(self.wrapped, attr)

    def write(self, data: bytes | bytearray | memoryview) -> None:
        # Write the data to the replay file
        self.replay_fp.write(data)
        # Pass on the data to the wrapped object
        return self.wrapped.write(data)


class ALSLanguageClientProtocol(LanguageClientProtocol):

    def connection_made(self, transport: asyncio.Transport):  # type: ignore

        if ALS_TEST_RECORD:
            # If recording is active, use a transport wrapper. Otherwise use the
            # original transport object.
            LOG.info("Replay file: %s", _replay.absolute())
            transport = RecordingTransportWrapper(transport, _replay)

        return super().connection_made(transport)


class OnTypeFormattingSetting(TypedDict):
    indentOnly: bool


class ALSSettings(
    TypedDict,
    # This indicates that the dictionary keys can be omitted, they are not required to
    # appear
    total=False,
):
    """This class helps create a dictionary of ALS settings. It has to be manually
    updated when new settings are added.

    So if you see a missing setting that you need, consider adding it.
    """

    defaultCharset: str | None
    displayMethodAncestryOnNavigation: bool | None
    documentationStyle: Literal["gnat", "leading"] | None
    adaFileDiagnostics: bool | None
    enableIndexing: bool | None
    foldComments: bool | None
    followSymlinks: bool | None
    insertWithClauses: bool | None
    logThreshold: int | None
    namedNotationThreshold: int | None
    onTypeFormatting: OnTypeFormattingSetting | None
    projectDiagnostics: bool | None
    projectFile: str | None
    gprConfigurationFile: str | None
    relocateBuildTree: str | None
    renameInComments: bool | None
    rootDir: str | None
    scenarioVariables: dict[str, str] | None
    useCompletionSnippets: bool | None
    useGnatformat: bool | None


class ALSLanguageClient(LanguageClient):
    """This class provides methods to communicate with the Ada Language Server."""

    def __init__(
        self,
        *args,
        configuration: lsprotocol.types.Dict[str, Any] | None = None,
        **kwargs,
    ):
        if "protocol_cls" not in kwargs:
            kwargs["protocol_cls"] = ALSLanguageClientProtocol

        super().__init__(*args, configuration=configuration, **kwargs)

    def debugHere(self, msg: str | None = None):
        """Pause the test execution and print the PID of the ALS process in order to
        attach a debugger. The test driver will ask the User to press Enter after the
        debugger is attached to continue test execution.
        """
        import psutil

        if not args.debug:
            raise TestInfraError(
                "Test must be run with --debug to use debugHere()",
                print_backtrace=False,
            )

        if self._server is None:
            raise TestInfraError(
                "Server object is null. Cannot determine PID to debug."
            )

        server = psutil.Process(self._server.pid)
        LOG.debug("Server process PID: %d, Name: %s", server.pid, server.name())

        # The server can either be the ALS process, or the lsp-devtools process wrapping
        # it
        if "ada_language_server" in server.name():
            LOG.debug("Server process is ALS")
            pass
        else:
            LOG.debug("Server process is not ALS. Let's find ALS.")
            # Find the child process of lsp-devtools that is the ALS
            children = server.children(True)

            if LOG.getEffectiveLevel() <= logging.DEBUG:
                # Only compute this in verbose mode to avoid the overhead
                children_info = "\n".join(f"   {p.pid}: {p.name()}" for p in children)
                LOG.debug("Children processes:\n%s", children_info)

            server = next((p for p in children if "ada_language_server" in p.name()))

        if not msg:
            msg = "## Debug point reached. Attach with:"
        print(msg, file=sys.stderr)
        print(f"    gdb -p {server.pid}", file=sys.stderr)
        print("", file=sys.stderr)
        print("## Press Enter to continue", file=sys.stderr)
        input()

    async def getCurrentProject(self) -> str | None:
        """Craft a request for the "als-project-file" command which queries for the
        currently loaded project.
        """
        return await self.workspace_execute_command_async(
            ExecuteCommandParams("als-project-file")
        )

    async def getObjectDir(self) -> str | None:
        """Send the "als-object-dir" command to obtain the object dir of the currently
        loaded project.
        """
        return await self.workspace_execute_command_async(
            ExecuteCommandParams("als-object-dir")
        )

    async def getObjDirBasename(self) -> str | None:
        """Send the "als-object-dir" command to obtain the object dir of the currently
        loaded project, and return its basename.
        """
        obj_dir = await self.getObjectDir()
        if obj_dir is not None:
            return os.path.basename(obj_dir)
        else:
            return None

    def didChangeConfig(self, settings: ALSSettings) -> None:
        """Send a workspace/didChangeConfiguration notification with as set of ALS
        settings.
        """
        self.workspace_did_change_configuration(
            DidChangeConfigurationParams(settings={"ada": settings})
        )

    def didOpenFile(self, src_path: Path | str, language_id="ada", version=0) -> str:
        """Send a textDocument/didOpen notification for a file on disk. The content of
        the file is automatically read and sent in the notification. The URI is returned
        for later use in other requests about the file.
        """
        uri = URI(src_path)

        self.text_document_did_open(
            DidOpenTextDocumentParams(
                TextDocumentItem(uri, language_id, version, Path(src_path).read_text())
            )
        )

        return uri

    def didOpenVirtual(
        self, uri: str | None = None, language_id="ada", version=0, text: str = ""
    ) -> str:
        """Send a didOpen notification for a file that doesn't exist on disk.

        If the `uri` parameter is omitted, a random one is generated
        automatically with a `.ads` extension.

        :param uri: the URI of the file. If None, that will be automatically
        generated and returned as a result of the call.
        :param language_id: the language_id parameter of the LSP notification.
        :param version: the version parameter of the LSP notification. Defaults
        to 0.
        :param text: the text parameter of the LSP notification.

        :return: the URI of the document
        """
        if uri is None:
            path = str(uuid.uuid4())
            if language_id == "ada":
                path += ".ads"
            else:
                path += "." + language_id
            uri = URI(path)

        self.text_document_did_open(
            DidOpenTextDocumentParams(TextDocumentItem(uri, language_id, version, text))
        )

        return uri

    async def awaitIndexingEnd(self):
        """Wait until the ALS finishes indexing."""
        LOG.info("Awaiting indexing start and end")

        indexing_progress = None
        while indexing_progress is None:
            await asyncio.sleep(0.2)
            if args.verbose >= 2:
                LOG.debug(
                    "Awaiting indexing progress - lsp.progress_reports = %s",
                    self.progress_reports,
                )
            indexing_progress = next(
                (prog for prog in self.progress_reports if "indexing" in str(prog)),
                None,
            )

        LOG.info("Received indexing progress token")

        last_progress = None
        while not isinstance(last_progress, WorkDoneProgressEnd):
            await asyncio.sleep(0.2)
            if args.verbose >= 2:
                LOG.debug(
                    "Waiting for indexing end - last_progress = %s", last_progress
                )
            # Initially the list of progress messages is empty, so check the length
            # before reading
            last_progress = (
                self.progress_reports[indexing_progress][-1]
                if self.progress_reports[indexing_progress]
                else None
            )

        LOG.info("Received indexing end message")

    def assertNoLSPErrors(self):
        """Assert that no Error-level log messages have been received by the LSP client
        so far.
        """
        errors = [m for m in self.log_messages if m.type == MessageType.Error]
        if errors:
            msg = "\n### Found LSP Errors ###\n" + to_str(errors)
            raise AssertionError(msg)

    async def prepareCallHierarchy(
        self, uri: str, line_one_based: int, char_one_based: int
    ):
        return await self.text_document_prepare_call_hierarchy_async(
            CallHierarchyPrepareParams(
                TextDocumentIdentifier(uri), Pos(line_one_based, char_one_based)
            )
        )

    async def callHierarchyIncomingCalls(
        self, uri: str, line_one_based: int, char_one_based: int
    ):
        rng = RangeZero(line_one_based, char_one_based)
        return await self.call_hierarchy_incoming_calls_async(
            CallHierarchyIncomingCallsParams(
                CallHierarchyItem(
                    name="",
                    kind=SymbolKind.Function,
                    uri=uri,
                    range=rng,
                    selection_range=rng,
                )
            )
        )

    async def references(self, uri: str, line_one_based: int, char_one_based: int):
        pos = Pos(line_one_based, char_one_based)
        return await self.text_document_references_async(
            lsprotocol.types.ReferenceParams(
                context=lsprotocol.types.ReferenceContext(include_declaration=True),
                text_document=TextDocumentIdentifier(uri),
                position=pos,
            )
        )

    def assertEqual(self, actual: Any, expected: Any) -> None:
        """Raise an AssertionError if actual != expected."""
        assertEqual(actual, expected)

    def assertLocationsList(
        self,
        actual: Sequence[CallHierarchyItem | CallHierarchyIncomingCall],
        expected: list[tuple[str, int]],
    ) -> None:
        """Assert the content of a list of results from a CallHierarchy or
        CallHierarchyIncomingCall request. Expected results are given as a list of
        (<basename>, <line-one-based>) tuples.
        """
        assertLocationsList(actual, expected)

    async def sleep(self, seconds: float) -> None:
        """Wait for the given amount of seconds multiplied by ALS_WAIT_FACTOR."""
        wait_factor: int = int(os.environ.get("ALS_WAIT_FACTOR", "1"))
        await asyncio.sleep(seconds * wait_factor)

    async def getProjectAttributeValue(
        self, attribute: str, pkg: str = "", index: str = ""
    ) -> str | list[str]:
        """Send a workspace/executeCommand request with command
        'als-get-project-attribute-value' to retrieve an attribute of the loaded
        project.
        """
        result = await self.workspace_execute_command_async(
            ExecuteCommandParams(
                command="als-get-project-attribute-value",
                arguments=[{"attribute": attribute, "pkg": pkg, "index": index}],
            )
        )

        assert isinstance(result, str) or (
            isinstance(result, list)
            and all(lambda x: isinstance(x, str) for x in result)
        ), f"Unexpected type of result: {result}"

        return result


def als_client_factory() -> ALSLanguageClient:
    """This function is an ugly copy-paste of pytest_lsp.make_test_lsp_client. It is
    necessary in order to override some aspects of the pytest_lsp.LanguageClient class,
    whilst making the other useful feature registrations that the original function
    makes.

    The aspect to override is the protocol class where we want to intervene to record
    messages to a replay file.

    I opened an issue to improve the situation so that the duplication can be avoided in
    the future:
    https://github.com/swyddfa/lsp-devtools/issues/195
    """

    # Instantiate the object from our own ALSLanguageClient class
    client = ALSLanguageClient(
        converter_factory=default_converter,
    )

    # The rest of the function is identical to the original, but we have to replace the
    # type of the client with our own in order for dynamic parameter matching to work.

    @client.feature(types.WORKSPACE_CONFIGURATION)
    def configuration(client: ALSLanguageClient, params: types.ConfigurationParams):
        return [
            client.get_configuration(section=item.section, scope_uri=item.scope_uri)
            for item in params.items
        ]

    @client.feature(types.TEXT_DOCUMENT_PUBLISH_DIAGNOSTICS)
    def publish_diagnostics(
        client: ALSLanguageClient, params: types.PublishDiagnosticsParams
    ):
        client.diagnostics[params.uri] = params.diagnostics

    @client.feature(types.WINDOW_WORK_DONE_PROGRESS_CREATE)
    def create_work_done_progress(
        client: ALSLanguageClient, params: types.WorkDoneProgressCreateParams
    ):
        if params.token in client.progress_reports:
            # TODO: Send an error reponse to the client - might require changes
            #       to pygls...
            warnings.warn(
                f"Duplicate progress token: {params.token!r}",
                LspSpecificationWarning,
                stacklevel=2,
            )

        client.progress_reports.setdefault(params.token, [])

    @client.feature(types.PROGRESS)
    def progress(client: ALSLanguageClient, params: types.ProgressParams):
        if params.token not in client.progress_reports:
            warnings.warn(
                f"Unknown progress token: {params.token!r}",
                LspSpecificationWarning,
                stacklevel=2,
            )

        if not params.value:
            return

        if (kind := params.value.get("kind", None)) == "begin":
            type_: Type[Any] = types.WorkDoneProgressBegin
        elif kind == "report":
            type_ = types.WorkDoneProgressReport
        elif kind == "end":
            type_ = types.WorkDoneProgressEnd
        else:
            raise TypeError(f"Unknown progress kind: {kind!r}")

        value = client.protocol._converter.structure(params.value, type_)
        client.progress_reports.setdefault(params.token, []).append(value)

    @client.feature(types.WINDOW_LOG_MESSAGE)
    def log_message(client: ALSLanguageClient, params: types.LogMessageParams):
        client.log_messages.append(params)

        levels = [logger.error, logger.warning, logger.info, logger.debug]
        levels[params.type.value - 1](params.message)

    @client.feature(types.WINDOW_SHOW_MESSAGE)
    def show_message(client: ALSLanguageClient, params):
        client.messages.append(params)

    @client.feature(types.WINDOW_SHOW_DOCUMENT)
    def show_document(
        client: ALSLanguageClient, params: types.ShowDocumentParams
    ) -> types.ShowDocumentResult:
        client.shown_documents.append(params)
        return types.ShowDocumentResult(success=True)

    return client


@attrs.define
class ALSClientServerConfig(ClientServerConfig):

    # Override the default client factory to our own which support recording a replay
    # file
    client_factory: Callable[[], JsonRPCClient] = attrs.field(
        default=als_client_factory,
    )


WRAPPER_ATTRIBUTE = "__wrapper"


def test(
    config: ALSClientServerConfig | None = None,
    initialize: bool = True,
    shutdown: bool = True,
    assert_no_lsp_errors: bool = True,
    als_settings: ALSSettings | None = None,
    timeout=15,
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
    :param als_settings: ALS settings to send as 'initializationOptions' with the
    'initialize' request. Only applicable if initialize=True (which is the default).
    :param timeout: test timeout in seconds
    """

    async def async_wrapper(
        func: Callable[[ALSLanguageClient], Awaitable[object]],
    ) -> None:
        als = os.environ.get("ALS", "ada_language_server")
        command = [als]
        conf = config or ALSClientServerConfig(command)
        client: ALSLanguageClient | None = None

        try:
            client = await conf.start()  # type: ignore
            assert client

            if args.debug:
                client.debugHere("## We're about to start the test. Attach with:")

            if initialize:
                await client.initialize_session(
                    params=InitializeParams(
                        ClientCapabilities(),
                        # ALS doesn't support the newer workspaceFolders property so we
                        # have to use the older rootURI property.
                        root_uri=URI(os.getcwd()),
                        initialization_options=(
                            {"ada": als_settings} if als_settings is not None else None
                        ),
                    )
                )

            LOG.info("Running test function: %s", func.__name__)

            actual_timeout = timeout
            if "ALS_WAIT_FACTOR" in os.environ:
                factor = int(os.environ["ALS_WAIT_FACTOR"])
                actual_timeout *= factor

            # Run the test with a timeout
            async with asyncio.timeout(actual_timeout):
                await func(client)

            if assert_no_lsp_errors:
                # Assert the absence of Error LSP log messages
                client.assertNoLSPErrors()
        finally:
            try:
                if client:
                    log_lsp_logs(client)
                    log_lsp_diagnostics(client)
                    if shutdown:
                        await client.shutdown_session()
            finally:
                if _replay.exists():
                    msg = "You can replay this test in a debugger using:\n\n"
                    msg += (
                        f"    gdb {shlex.quote(conf.server_command[0])}"
                        f" --cd={shlex.quote(os.getcwd())}"
                    )
                    if len(conf.server_command) > 1:
                        msg += " -- " + " ".join(
                            shlex.quote(arg) for arg in conf.server_command[1:]
                        )
                    msg += "\n"
                    msg += f"    (gdb) run < {_replay.absolute()}"
                    msg += "\n"

                    LOG.info(msg)

    def wrapper(func: Callable[[ALSLanguageClient], Awaitable[object]]):
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
            f"No functions with @{__name__}.{test.__name__}()"
            " decorator found in {test_py_path}"
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
    try:
        do_main()
    except TestInfraError as ex:
        if ex.print_backtrace:
            # Simply re-raise to obtain the standard Python backtrace
            raise
        else:
            # Just print the exception message and exit with a non-zero code
            LOG.critical("%s", ex)
            sys.exit(1)


def do_main():
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
    p.add_argument(
        "--debug",
        action="store_true",
        help="Run a test in debug mode where stdin and stdout"
        " are not piped to allow for interactions.",
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


def Pos(line_one_based: int, char_one_based: int) -> Position:
    """Shortcut for creating a Position object with ONE-BASED locations."""
    return Position(line_one_based - 1, char_one_based - 1)


def RangeZero(line_one_based: int, char_one_based: int) -> Range:
    """Shortcut for creating a Range that starts and ends at the same location given
    with ONE-BASED integers.
    """
    pos = Pos(line_one_based, char_one_based)
    return Range(pos, pos)


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

    # Replace back slashes with forward slashes to avoid too much quoting in URIs
    src_abs_str = str(src_abs).replace("\\", "/")

    quoted = urllib.parse.quote(src_abs_str)

    if not quoted.startswith("/"):
        # Make sure the path starts with /, even on Windows
        #
        # TODO look for documentation that explains this
        quoted = "/" + quoted

    src_uri = f"file://{quoted}"

    if args.verbose >= 2:
        LOG.debug("Converted %s to URI: %s", src_path, src_uri)

    return src_uri


def find_ALS_process():
    """
    Return the ALS process launched by the pyslsp test driver.
    """
    for x in psutil.pids():
        try:
            p = psutil.Process(x)
            if p.ppid() == os.getpid() and p.name().startswith("ada_language_server"):
                return p
        except psutil.NoSuchProcess:
            pass


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


def get_ALS_memory_footprint():
    """
    Return the memory footprint in MB of the ALS process launched
    by the pyslsp test driver.
    """
    for x in psutil.pids():
        try:
            p = psutil.Process(x)
            if p.ppid() == os.getpid() and p.name().startswith("ada_language_server"):
                return p.memory_full_info().rss / (1024**2)
        except psutil.NoSuchProcess:
            pass


class TestInfraError(Exception):

    def __init__(
        self, msg: str | None = None, print_backtrace: bool = True, *args: object
    ) -> None:
        super().__init__(msg, *args)
        self.print_backtrace = print_backtrace
