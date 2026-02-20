"""
Test that texDocument/formatting does not crash the server
when formatting huge files.
"""

from drivers.pylsp import ALSLanguageClient, test
from lsprotocol.types import (
    DocumentFormattingParams,
    FormattingOptions,
    HoverParams,
    Position,
    TextDocumentIdentifier,
)


@test(timeout=30)
async def test_called_by(lsp: ALSLanguageClient):
    # Send a didOpen for libadalang-analysis.adb
    uri = lsp.didOpenFile("libadalang-analysis.adb")

    # Send a textDocument/formatting request, on the whole file
    await sendFormattingRequest(lsp, uri)

    # Check that the server is still alive
    result = await lsp.text_document_hover_async(
        HoverParams(
            TextDocumentIdentifier(uri),
            Position(line=347, character=15),
        )
    )
    assert (
        result is not None
    ), "textDocument/hover request failed after formatting huge file"


async def sendFormattingRequest(lsp, uri):
    """
    Send a textDocument/formatting request.
    """
    # Send a textDocument/formatting request, on the whole file this time
    await lsp.text_document_formatting_async(
        DocumentFormattingParams(
            TextDocumentIdentifier(uri),
            FormattingOptions(tab_size=3, insert_spaces=False),
        )
    )
