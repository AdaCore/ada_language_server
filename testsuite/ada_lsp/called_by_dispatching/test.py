"""test that callHierarchy/incomingCalls works for dispatching calls"""

from drivers.lsp_python_driver import simple_test
from drivers.lsp_ada_requests import didOpen_from_disk
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
    # This is how to craft a request by hand
    # TODO: add a helper function to lsp_ada_requests.py to craft this request
    message = LSPMessage(
        {
            "method": "textDocument/prepareCallHierarchy",
            "params": {
                "textDocument": {"uri": URI(main_adb)},
                "position": {"line": 6, "character": 3},
            },
        }
    )
    response = lsp.send(message)

    # Expect exactly this result
    response.assertEquals(
        [
            {
                "name": "foo",
                "kind": 12,
                "detail": "at root.ads (5:14)",
                "uri": URI(root_ads),
                "range": {
                    "start": {"line": 4, "character": 3},
                    "end": {"line": 4, "character": 30},
                },
                "selectionRange": {
                    "start": {"line": 4, "character": 13},
                    "end": {"line": 4, "character": 16},
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
                    "uri": URI(root_ads),
                    "range": {
                        "start": {"line": 4, "character": 13},
                        "end": {"line": 4, "character": 13},
                    },
                    "selectionRange": {
                        "start": {"line": 4, "character": 13},
                        "end": {"line": 4, "character": 13},
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
    print(results)

    assert results == [("main", "main.adb", 2)]
