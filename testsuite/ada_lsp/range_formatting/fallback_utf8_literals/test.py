"""Test the rangeFormatting fallback on incomplete UTF-8 literals"""

import os

from drivers.pylsp import URI, ALSLanguageClient, test
from lsprotocol.types import (
    ClientCapabilities,
    DocumentRangeFormattingParams,
    FormattingOptions,
    InitializedParams,
    InitializeParams,
    Position,
    Range,
    TextDocumentIdentifier,
    TextEdit,
)


@test(initialize=False)
async def test_range_formatting_utf8_literals(lsp: ALSLanguageClient) -> None:
    """
    Test the rangeFormatting feature on buffers the Ada parser cannot parse,
    so that the legacy fallback indenter is the one answering.

    Each source holds one literal that is not terminated before the end of its
    line, and that line ends with a multi-byte character. The fallback
    indenter used to scan such a literal past the end of the line and leave
    its scan pointer on the line terminator; the enclosing loop then stepped
    over that terminator without counting the line. A whole-buffer range shows
    that directly: one line short in the answer, and everything after the
    literal indented from a stale line. str_open_paren.adb is the third
    variant, where the opening delimiter is itself the last character of the
    line: the recovery that resets the parentheses stack used to be skipped,
    and every following line was dragged out to the column of the parenthesis
    left open.
    """

    failed_tests = []

    await lsp.initialize_session(
        InitializeParams(
            capabilities=ClientCapabilities(),
            root_uri=URI(os.getcwd()),
        )
    )

    lsp.initialized(InitializedParams())
    lsp.didChangeConfig({"rangeFormattingFallback": True})
    await lsp.awaitIndexingEnd()

    #  All three sources have the same shape, so they are reindented the same
    #  way: the declaration on line 1 at one indentation level, `begin` back
    #  at the level of the declaration, then `if` / `null;` / `end if;`.

    expected_text_edits = [
        TextEdit(Range(Position(0, 0), Position(0, 0)), ""),
        TextEdit(Range(Position(1, 0), Position(1, 3)), " " * 3),
        TextEdit(Range(Position(2, 0), Position(2, 0)), " " * 3),
        TextEdit(Range(Position(3, 0), Position(3, 3)), " " * 6),
        TextEdit(Range(Position(4, 0), Position(4, 6)), " " * 9),
        TextEdit(Range(Position(5, 0), Position(5, 3)), " " * 6),
    ]

    for filename in (
        "str_multibyte.adb",
        "chr_multibyte.adb",
        "str_open_paren.adb",
    ):
        try:
            uri = lsp.didOpenFile(os.path.join(os.getcwd(), filename))
            text_edits = await lsp.text_document_range_formatting_async(
                DocumentRangeFormattingParams(
                    TextDocumentIdentifier(uri),
                    Range(Position(0, 0), Position(5, 0)),
                    FormattingOptions(tab_size=3, insert_spaces=True),
                )
            )
            lsp.assertEqual(text_edits, expected_text_edits)
        except Exception as e:  # pylint: disable=broad-exception-caught
            failed_tests.append(f"Test case '{filename}' failed:\n{str(e)}")

    if len(failed_tests) > 0:
        fail_messages = "\n\n".join(failed_tests)
        message = f"Range formatting tests failed\n\n{fail_messages}"
        raise Exception(message)  # pylint: disable=broad-exception-raised
