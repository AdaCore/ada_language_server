"""
Test that textDocument/formatting normalizes identifier casing when the
project's Format package sets `Identifier_Casing` to `definition`.

"""

from drivers.pylsp import ALSLanguageClient, assertEqual, test
from lsprotocol.types import (
    DocumentFormattingParams,
    FormattingOptions,
    TextDocumentIdentifier,
    TextEdit,
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


def apply_text_edits(text: str, edits: list[TextEdit]) -> str:
    """Apply a list of non-overlapping LSP TextEdits to `text`.

    Args:
        text (str): The original document text.
        edits (list[TextEdit]): Edits to apply, as returned by the server.

    Returns:
        str: The document text with all edits applied.
    """

    lines = text.splitlines(keepends=True)

    def offset(line: int, character: int) -> int:
        return sum(len(lines[i]) for i in range(min(line, len(lines)))) + (
            character if line < len(lines) else 0
        )

    # Apply edits from the end of the document so that earlier offsets stay
    # valid as the text is spliced.
    for edit in sorted(
        edits,
        key=lambda e: (e.range.start.line, e.range.start.character),
        reverse=True,
    ):
        start = offset(edit.range.start.line, edit.range.start.character)
        end = offset(edit.range.end.line, edit.range.end.character)
        text = text[:start] + edit.new_text + text[end:]

    return text
