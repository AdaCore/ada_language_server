from drivers.lsp_python_driver import LSP, complex_test
from drivers.lsp_ada_requests import (
    didOpen_from_disk,
    prepareCallHierarchy,
    incomingCalls,
    initialize,
    initialized,
    didChangeConfiguration,
)
from drivers.lsp_types import LSPMessage, URI
import os


# Use a complex_test rather than a simple_test
# so we can inspect the results of initialize()
@complex_test
def test_called_by(wd) -> list[str] | None:
    program = os.environ.get("ALS", "ada_language_server")
    lsp = LSP(program, wd)
    # Send the initialize request
    response = lsp.send(initialize())
    # Verify that the right capability is advertised
    response.assertField("capabilities.callHierarchyProvider", True)

    # Send the initialized notification and the didChangeConfiguration notification
    lsp.send(initialized())
    lsp.send(didChangeConfiguration())
    # Send a didOpen for main.adb
    main_adb = os.path.join(wd, "main.adb")
    p_adb = os.path.join(wd, "p.adb")

    lsp.send(didOpen_from_disk(main_adb))

    # Send a textDocument/prepareCallHierarchy request
    response = lsp.send(prepareCallHierarchy(main_adb, 3, 44))
    response.assertLocationsList([("p.adb", 2)])

    # Now send the callHierarchy/incomingCalls request
    response = lsp.send(incomingCalls(p_adb, 2, 14))

    # Expect these locations
    response.assertLocationsList(
        [
            ("main.adb", 10),
            ("main.adb", 14),
            ("p.adb", 2),
            ("main.adb", 2),
            ("p.ads", 1),
            ("p.adb", 8),
        ]
    )
    lsp.shutdown()
    return []