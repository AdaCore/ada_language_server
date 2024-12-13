import os

from drivers.pylsp import URI, ALSLanguageClient, test
from lsprotocol.types import ClientCapabilities, InitializedParams, InitializeParams


# Use initialize=False so we can inspect the results of the initialize request
@test(initialize=False)
async def test_called_by(lsp: ALSLanguageClient) -> None:
    # Send the initialize request
    response = await lsp.initialize_session(
        InitializeParams(ClientCapabilities(), root_uri=URI(os.getcwd()))
    )
    # Verify that the right capability is advertised
    lsp.assertEqual(response.capabilities.call_hierarchy_provider, True)

    # Send the initialized notification and the didChangeConfiguration notification
    lsp.initialized(InitializedParams())
    lsp.didChangeConfig({})

    # Send a didOpen for main.adb
    main_adb = lsp.didOpenFile("main.adb")

    # Send a textDocument/prepareCallHierarchy request
    res1 = await lsp.prepareCallHierarchy(main_adb, 3, 44)
    lsp.assertLocationsList(res1, [("p.adb", 2)])

    # Now send the callHierarchy/incomingCalls request
    p_adb = URI("p.adb")
    res2 = await lsp.callHierarchyIncomingCalls(p_adb, 2, 14)

    # Expect these locations
    lsp.assertLocationsList(
        res2,
        [
            ("main.adb", 10),
            ("main.adb", 14),
            ("p.adb", 2),
            ("main.adb", 2),
            ("p.ads", 1),
            ("p.adb", 8),
        ],
    )
