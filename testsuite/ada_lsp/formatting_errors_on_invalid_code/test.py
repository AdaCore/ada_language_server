"""
Test that texDocument/formatting and textDocument/rangeFormatting requests
return errors when the code is syntactically invalid.
"""

import pygls.exceptions
from drivers.pylsp import ALSLanguageClient, assertEqual, test
from lsprotocol.types import (
    DidChangeConfigurationParams,
    DocumentFormattingParams,
    DocumentRangeFormattingParams,
    FormattingOptions,
    Position,
    Range,
    TextDocumentIdentifier,
)


@test()
async def test_called_by(lsp: ALSLanguageClient):
    # Send a didOpen for main.adb
    main_adb_uri = lsp.didOpenFile("main.adb")

    # Test with GNATformat formatting backend
    lsp.workspace_did_change_configuration(
        DidChangeConfigurationParams({"ada": {"useGnatformat": True}})
    )
    await sendFormattingRequestsAndAssertErrors(lsp, main_adb_uri, "GNATformat")


async def sendFormattingRequestsAndAssertErrors(lsp, main_adb_uri, formatter):
    """
    Send a textDocument/rangeFormatting and textDocument/formatting requests and
    check if we get an error.
    """
    try:
        await lsp.text_document_range_formatting_async(
            DocumentRangeFormattingParams(
                TextDocumentIdentifier(main_adb_uri),
                Range(Position(4, 0), Position(3, 0)),
                FormattingOptions(tab_size=3, insert_spaces=False),
            )
        )
        raise AssertionError(
            "Expected a JsonRpcException when formatting "
            "invalid code (LSP error), but did not get one"
        )

    except pygls.exceptions.JsonRpcException as e:
        error_message = e.message

    assertEqual(
        error_message, "%s: Syntactically invalid code can't be formatted" % formatter
    )

    # Send a textDocument/formatting request, on the whole file this time
    try:
        await lsp.text_document_formatting_async(
            DocumentFormattingParams(
                TextDocumentIdentifier(main_adb_uri),
                FormattingOptions(tab_size=3, insert_spaces=False),
            )
        )
        raise AssertionError(
            "Expected a JsonRpcException when formatting "
            "invalid code (LSP error), but did not get one"
        )

    except pygls.exceptions.JsonRpcException as e:
        error_message = e.message

    assertEqual(
        error_message, "%s: Syntactically invalid code can't be formatted" % formatter
    )
