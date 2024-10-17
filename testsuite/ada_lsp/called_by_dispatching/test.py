"""test that callHierarchy/incomingCalls works for dispatching calls"""

from drivers.lsp_python_driver import simple_test
from drivers.lsp_ada_requests import didOpen_from_disk, prepareCallHierarchy, incomingCalls
from drivers.lsp_types import LSPMessage, URI
import os


@simple_test
def test_called_by(lsp, wd):
    # Send a didOpen for main.adb
    main_adb = os.path.join(wd, "main.adb")
    root_ads = os.path.join(wd, "root.ads")
    p_adb = os.path.join(wd, "p.adb")

    lsp.send(didOpen_from_disk(main_adb))

    # Send a textDocument/prepareCallHierarchy request
    response = lsp.send(prepareCallHierarchy(main_adb, 7, 4))

    # Expect these locations
    response.assertLocationsList([("root.ads",5)])

    # Now send the callHierarchy/incomingCalls request
    response = lsp.send(incomingCalls(root_ads, 5, 14))

    # Expect these locations
    response.assertLocationsList([("main.adb", 3)])
