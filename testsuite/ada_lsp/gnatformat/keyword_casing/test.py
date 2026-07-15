"""
Test all the casing options for keywords defined by GNATFormat.
"""

from drivers.pylsp import ALSLanguageClient, assertEqual, test, GnatFormattingOptions
from drivers.lsp_ada_requests import apply_text_edits
from lsprotocol.types import (
    DocumentFormattingParams,
    TextDocumentIdentifier,
)


@test(als_settings={"projectFile": "default.gpr"})
async def test_identifier_casing(lsp: ALSLanguageClient):
    uri = lsp.didOpenFile("main.adb")

    # The default casings is Keep for both keywords and identifier
    edits = await lsp.text_document_formatting_async(
        DocumentFormattingParams(
            TextDocumentIdentifier(uri),
            options=GnatFormattingOptions(tab_size=3, insert_spaces=True),
        )
    )

    # We are getting no edits for Keep
    assertEqual(edits, None)

    # Test Lower
    edits = await lsp.text_document_formatting_async(
        DocumentFormattingParams(
            TextDocumentIdentifier(uri),
            options=GnatFormattingOptions(
                tab_size=3,
                insert_spaces=True,
                gnatKeywordCasing="Lower",
                gnatIdentifierCasing="Upper",
            ),
        )
    )

    assert edits, "expected textDocument/formatting to return edits for Lower"

    formatted = apply_text_edits(open("main.adb").read(), edits)

    assertEqual(
        formatted,
        "procedure MAIN is\n"
        "   MY_VARIABLE : INTEGER := 0;\n"
        "begin\n"
        "   MY_VARIABLE := MY_VARIABLE + 1;\n"
        "end MAIN;\n",
    )

    # Test Upper
    edits = await lsp.text_document_formatting_async(
        DocumentFormattingParams(
            TextDocumentIdentifier(uri),
            options=GnatFormattingOptions(
                tab_size=3,
                insert_spaces=True,
                gnatKeywordCasing="Upper",
                gnatIdentifierCasing="Upper",
            ),
        )
    )

    assert edits, "expected textDocument/formatting to return edits for Upper"

    formatted = apply_text_edits(open("main.adb").read(), edits)

    assertEqual(
        formatted,
        "PROCEDURE MAIN IS\n"
        "   MY_VARIABLE : INTEGER := 0;\n"
        "BEGIN\n"
        "   MY_VARIABLE := MY_VARIABLE + 1;\n"
        "END MAIN;\n",
    )
