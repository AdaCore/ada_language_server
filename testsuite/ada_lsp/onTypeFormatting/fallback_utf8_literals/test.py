"""Test the onTypeFormatting fallback on incomplete UTF-8 literals"""

import os
from drivers.pylsp import URI, ALSLanguageClient, OnTypeFormattingSetting, test
from drivers.lsp_ada_requests import run_indentation_testcases, IndentationTestCase
from lsprotocol.types import (
    ClientCapabilities,
    FormattingOptions,
    InitializedParams,
    InitializeParams,
    Position,
)


@test(initialize=False)
async def test_on_type_formatting_utf8_literals(lsp: ALSLanguageClient) -> None:
    """
    Test the onTypeFormatting feature on buffers the Ada parser cannot parse,
    so that the legacy fallback indenter is the one answering.

    Each source holds one literal that is not terminated before the end of its
    line, and that line ends with a multi-byte character. The fallback
    indenter used to scan such a literal past the end of the line and leave
    its scan pointer on the line terminator; the enclosing loop then stepped
    over that terminator without counting the line, and every line after it
    was indented from a stale line number. str_open_paren.adb is the third
    variant: nothing is scanned past at all, because the opening delimiter is
    itself the last character of the line, and the recovery that resets the
    parentheses stack used to be skipped -- leaving the parenthesis open and
    dragging what follows out to its column.

    None of these buffers parses, so the indentation is not "correct" Ada in
    any strong sense. What is pinned down is that every line is still counted:
    the declaration continues at one continuation level, and the statement
    after `then` is indented from `if`, rather than the request coming back
    with no indentation at all or with the column of an unclosed parenthesis.
    """

    await lsp.initialize_session(
        InitializeParams(
            capabilities=ClientCapabilities(),
            root_uri=URI(os.getcwd()),
        )
    )

    lsp.initialized(InitializedParams())
    lsp.didChangeConfig({"onTypeFormatting": OnTypeFormattingSetting(indentOnly=True)})
    await lsp.awaitIndexingEnd()

    indentation_tests = [
        IndentationTestCase(
            "Unterminated string ending on a multi-byte character",
            "str_multibyte.adb",
            Position(1, 35),
            " " * 5,
        ),
        IndentationTestCase(
            "Statement after the unterminated string",
            "str_multibyte.adb",
            Position(3, 16),
            " " * 9,
        ),
        IndentationTestCase(
            "Incomplete character literal ending on a multi-byte character",
            "chr_multibyte.adb",
            Position(1, 23),
            " " * 5,
        ),
        IndentationTestCase(
            "Statement after the incomplete character literal",
            "chr_multibyte.adb",
            Position(3, 16),
            " " * 9,
        ),
        IndentationTestCase(
            "Opening quote as the last character of the line",
            "str_open_paren.adb",
            Position(1, 24),
            " " * 5,
        ),
        IndentationTestCase(
            "Statement after the unclosed parenthesis",
            "str_open_paren.adb",
            Position(3, 16),
            " " * 9,
        ),
    ]

    failed_tests = await run_indentation_testcases(
        lsp, indentation_tests, FormattingOptions(tab_size=3, insert_spaces=True)
    )

    if len(failed_tests) > 0:
        fail_messages = "\n\n".join(failed_tests)
        message = f"Indentation tests failed\n\n{fail_messages}"
        raise Exception(message)  # pylint: disable=broad-exception-raised
