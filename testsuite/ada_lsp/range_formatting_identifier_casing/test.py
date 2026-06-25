"""
Test that textDocument/rangeFormatting normalizes identifier casing when the
project's Format package sets `Identifier_Casing` to `definition`.

Only occurrences within the selected range should be recased: here the fourth
line (`my_variable := 1;`) is selected, so it becomes `My_Variable := 1;`,
while the unselected fifth line (`MY_VARIABLE := 2;`) is left untouched.
"""

from drivers.pylsp import ALSLanguageClient, assertEqual, test
from lsprotocol.types import (
    DocumentRangeFormattingParams,
    FormattingOptions,
    Position,
    Range,
    TextDocumentIdentifier,
    TextEdit,
)


@test(als_settings={"projectFile": "default.gpr"})
async def test_range_identifier_casing(lsp: ALSLanguageClient):
    uri = lsp.didOpenFile("main.adb")

    edits = await lsp.text_document_range_formatting_async(
        DocumentRangeFormattingParams(
            TextDocumentIdentifier(uri),
            Range(Position(3, 0), Position(4, 0)),
            FormattingOptions(tab_size=3, insert_spaces=True),
        )
    )

    assert edits, "expected textDocument/rangeFormatting to return edits"

    formatted = apply_text_edits(open("main.adb").read(), edits)

    assertEqual(
        formatted,
        "procedure Main is\n"
        "   My_Variable : Integer := 0;\n"
        "begin\n"
        "   My_Variable := 1;\n"
        "   MY_VARIABLE := 2;\n"
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
