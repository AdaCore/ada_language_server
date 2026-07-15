"""
Test all the casing options for identifier defined by GNATFormat.
"""

from drivers.pylsp import ALSLanguageClient, assertEqual, GnatFormattingOptions, test
from drivers.lsp_ada_requests import apply_text_edits
from lsprotocol.types import (
    DocumentFormattingParams,
    FormattingOptions,
    TextDocumentIdentifier,
)


@test(als_settings={"projectFile": "default.gpr"})
async def test_identifier_casing(lsp: ALSLanguageClient):
    uri = lsp.didOpenFile("main.adb")

    # The default casings is Keep for both keywords and identifier
    edits = await lsp.text_document_formatting_async(
        DocumentFormattingParams(
            TextDocumentIdentifier(uri),
            FormattingOptions(tab_size=3, insert_spaces=True),
        )
    )

    # We are getting no edits for Keep
    assertEqual(edits, None)

    # Test Lower
    edits = await lsp.text_document_formatting_async(
        DocumentFormattingParams(
            TextDocumentIdentifier(uri),
            GnatFormattingOptions(
                tab_size=3,
                insert_spaces=True,
                gnatIdentifierCasing="Lower",
                gnatKeywordCasing="Upper",
            ),
        )
    )

    assert edits, "expected textDocument/formatting to return edits for Lower"
    print(edits)
    formatted = apply_text_edits(open("main.adb").read(), edits)

    assertEqual(
        formatted,
        "PROCEDURE main IS\n"
        "   my_variable : integer := 0;\n"
        "BEGIN\n"
        "   my_variable := my_variable + 1;\n"
        "END main;\n",
    )

    # Test Upper
    edits = await lsp.text_document_formatting_async(
        DocumentFormattingParams(
            TextDocumentIdentifier(uri),
            GnatFormattingOptions(
                tab_size=3,
                insert_spaces=True,
                gnatIdentifierCasing="Upper",
                gnatKeywordCasing="Upper",
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

    # Test Mixed
    edits = await lsp.text_document_formatting_async(
        DocumentFormattingParams(
            TextDocumentIdentifier(uri),
            GnatFormattingOptions(
                tab_size=3,
                insert_spaces=True,
                gnatIdentifierCasing="Mixed",
                gnatKeywordCasing="Upper",
            ),
        )
    )

    assert edits, "expected textDocument/formatting to return edits for Mixed"

    formatted = apply_text_edits(open("main.adb").read(), edits)

    assertEqual(
        formatted,
        "PROCEDURE Main IS\n"
        "   My_Variable : Integer := 0;\n"
        "BEGIN\n"
        "   My_Variable := My_Variable + 1;\n"
        "END Main;\n",
    )

    # Test Definition
    edits = await lsp.text_document_formatting_async(
        DocumentFormattingParams(
            TextDocumentIdentifier(uri),
            GnatFormattingOptions(
                tab_size=3,
                insert_spaces=True,
                gnatIdentifierCasing="Definition",
                gnatKeywordCasing="Upper",
            ),
        )
    )

    assert edits, "expected textDocument/formatting to return edits for Def"

    formatted = apply_text_edits(open("main.adb").read(), edits)

    assertEqual(
        formatted,
        "PROCEDURE Main IS\n"
        "   My_VaRiAbLe : Integer := 0;\n"
        "BEGIN\n"
        "   My_VaRiAbLe := My_VaRiAbLe + 1;\n"
        "END Main;\n",
    )
