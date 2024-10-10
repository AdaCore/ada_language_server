from drivers.lsp_python_driver import simple_test
from drivers.lsp_ada_requests import didOpen_from_disk, prepareCallHierarchy, incomingCalls
from drivers.lsp_types import LSPMessage, URI
import os


@simple_test
def test_called_by(lsp, wd):
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
    response.assertLocationsList([
        ("main.adb", 10),
        ("main.adb", 14),
        ("p.adb", 2),
        ("main.adb", 2),
        ("p.ads", 1),
        ("p.adb", 8),
    ])
