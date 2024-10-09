from lsp_python_driver import LSP
from lsp_ada_requests import initialize, initialized, didChangeConfiguration, didOpen_from_disk
from lsp_types import LSPMessage, URI
import os

def test_called_by():
    lsp = LSP()
    response = lsp.send(initialize())

    # Check that the server supports the callHierarchyProvider capability
    response.assertContains({"capabilities": {"callHierarchyProvider": True}})

    # Send the "initialized" notification
    lsp.send(initialized())

    # Send a didChangeConfiguration
    lsp.send(didChangeConfiguration())

    # Send a didOpen for main.adb
    lsp.send(didOpen_from_disk("main.adb"))

    # Send a textDocument/prepareCallHierarchy request
    # This is how to craft a request by hand
    # TODO: add a helper function to lsp_ada_requests.py to craft this request
    message = LSPMessage(
        {
            "method": "textDocument/prepareCallHierarchy",
            "params": {
                "textDocument": {"uri": URI("main.adb")},
                "position": {"line": 2, "character": 43},
            },
        }
    )
    response = lsp.send(message)

    # Expect exactly this result
    response.assertEquals(
        [
            {
                "name": "Foo",
                "kind": 12,
                "detail": "at p.adb (2:13)",
                "uri": URI("p.adb"),
                "range": {
                    "start": {"line": 1, "character": 3},
                    "end": {"line": 4, "character": 11},
                },
                "selectionRange": {
                    "start": {"line": 1, "character": 12},
                    "end": {"line": 1, "character": 15},
                },
            }
        ]
    )

    # Now send the callHierarchy/incomingCalls request
    # TODO: add a helper function to lsp_ada_requests.py to craft this request
    message = LSPMessage(
        {
            "method": "callHierarchy/incomingCalls",
            "params": {
                "item": {
                    "name": "",
                    "kind": 12,
                    "uri": URI("p.adb"),
                    "range": {
                        "start": {"line": 1, "character": 3},
                        "end": {"line": 4, "character": 11},
                    },
                    "selectionRange": {
                        "start": {"line": 1, "character": 12},
                        "end": {"line": 1, "character": 15},
                    },
                }
            },
        }
    )
    response = lsp.send(message)

    # Capture only the relevant fields
    # TODO: this could also be a small helper function
    results = []
    for item in response.from_dict:
        (name, file, line) = (
            item["from"]["name"],
            os.path.basename(item["from"]["uri"]),
            item["from"]["range"]["start"]["line"],
        )
        results.append((name, file, line))

    assert results == [
        ("Bla", "main.adb", 9),
        ("Bla", "main.adb", 13),
        ("Foo", "p.adb", 1),
        ("Main", "main.adb", 1),
        ("P", "p.ads", 0),
        ("T", "p.adb", 7),
    ]

    lsp.shutdown()


test_called_by()
