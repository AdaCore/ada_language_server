# The goal of this test is to check that if the initialize request includes a progress
# reporting token, then progress is reported by the ALS.
import os

from drivers import pylsp
from drivers.pylsp import URI, to_str
from lsprotocol.types import ClientCapabilities, InitializeParams, InitializedParams


@pylsp.test(initialize=False)
async def test(lsp: pylsp.ALSLanguageClient):
    lsp.capabilities = ClientCapabilities()
    progress_token = "init-progress"
    result = await lsp.initialize_async(
        InitializeParams(
            capabilities=lsp.capabilities,
            root_uri=URI(os.getcwd()),
            initialization_options={},
            # Include a progress token to check that we receive progress reports during
            # the initialize request
            work_done_token=progress_token,
        )
    )

    # Assert that we received progress reports
    lsp.assertEqual(
        to_str(lsp.progress_reports.get(progress_token, None)),
        """
[
  {
    "title": "Initializing Ada Language Server",
    "kind": "begin",
    "cancellable": false
  },
  {
    "kind": "end"
  }
]
""".strip(),
    )

    # Assert a non-null result
    lsp.assertNotEqual(result, None)

    lsp.initialized(InitializedParams())

    lsp.assertEqual(await lsp.getCurrentProject(), URI("hello.gpr"))
