"""
Test that textDocument/formatting normalizes identifier casing when the
project's Format package sets `Identifier_Casing` to `definition`.

"""

from drivers.pylsp import ALSLanguageClient, assertEqual, test
from drivers.lsp_ada_requests import apply_text_edits
from lsprotocol.types import (
    DocumentFormattingParams,
    FormattingOptions,
    TextDocumentIdentifier,
)


@test(als_settings={"projectFile": "default.gpr"})
async def test_identifier_casing(lsp: ALSLanguageClient):
    """
    Each identifier occurrence should be recased to match the spelling of its
    canonical defining name (here `My_Variable`).
    """

    uri = lsp.didOpenFile("main.adb")

    edits = await lsp.text_document_formatting_async(
        DocumentFormattingParams(
            TextDocumentIdentifier(uri),
            FormattingOptions(tab_size=3, insert_spaces=True),
        )
    )

    assert edits, "expected textDocument/formatting to return edits"

    formatted = apply_text_edits(open("main.adb").read(), edits)

    assertEqual(
        formatted,
        "procedure Main is\n"
        "   My_Variable : Integer := 0;\n"
        "begin\n"
        "   My_Variable := My_Variable + 1;\n"
        "end Main;\n",
    )
