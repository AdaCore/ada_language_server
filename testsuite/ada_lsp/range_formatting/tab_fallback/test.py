"""Test the rangeFormatting with tabs when there are diagnostics"""

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
    VersionedTextDocumentIdentifier,
)


@test(initialize=False)
async def test_on_type_formatting_indentation(lsp: ALSLanguageClient) -> None:

    failed_tests = []

    # Send the initialize request
    await lsp.initialize_session(
        InitializeParams(
            capabilities=ClientCapabilities(),
            root_uri=URI(os.getcwd()),
        )
    )

    # Send the initialized notification and the didChangeConfiguration
    # notification
    lsp.initialized(InitializedParams())
    lsp.didChangeConfig({"rangeFormattingFallback": True})
    await lsp.awaitIndexingEnd()
    source = VersionedTextDocumentIdentifier(
        uri=lsp.didOpenFile(os.path.join(os.getcwd(), "t.adb")), version=0
    )

    async def test_case(description, tested_range, expected_text_edits):
        try:
            text_edits = await lsp.text_document_range_formatting_async(
                DocumentRangeFormattingParams(
                    TextDocumentIdentifier(source.uri),
                    tested_range,
                    FormattingOptions(tab_size=3, insert_spaces=False),
                )
            )
            lsp.assertEqual(text_edits, expected_text_edits)
        except Exception as e:  # pylint: disable=broad-exception-caught
            failed_tests.append(f"Test case '{description}' failed:\n{str(e)}")

    await test_case(
        "Indentation when block",
        Range(Position(21, 0), Position(22, 0)),
        [
            TextEdit(
                Range(Position(21, 0), Position(21, 0)),
                "\t",
            ),
            TextEdit(
                Range(Position(22, 0), Position(22, 0)),
                "\t\t",
            ),
        ],
    )

    await test_case(
        "Indentation case block",
        Range(Position(14, 0), Position(16, 0)),
        [
            TextEdit(
                Range(Position(14, 0), Position(14, 0)),
                "\t",
            ),
            TextEdit(
                Range(Position(15, 0), Position(15, 0)),
                "\t",
            ),
            TextEdit(
                Range(Position(16, 0), Position(16, 0)),
                "\t\t",
            ),
        ],
    )

    await test_case(
        "Indentation case for+if",
        Range(Position(8, 0), Position(12, 0)),
        [
            TextEdit(
                Range(Position(8, 0), Position(8, 0)),
                "\t",
            ),
            TextEdit(
                Range(Position(9, 0), Position(9, 0)),
                "\t\t",
            ),
            TextEdit(
                Range(Position(10, 0), Position(10, 0)),
                "\t\t\t",
            ),
            TextEdit(
                Range(Position(11, 0), Position(11, 0)),
                "\t\t",
            ),
            TextEdit(
                Range(Position(12, 0), Position(12, 0)),
                "\t",
            ),
        ],
    )

    await test_case(
        "Indentation case if",
        Range(Position(4, 0), Position(6, 0)),
        [
            TextEdit(
                Range(Position(4, 0), Position(4, 0)),
                "\t",
            ),
            TextEdit(
                Range(Position(5, 0), Position(5, 0)),
                "\t\t",
            ),
            TextEdit(
                Range(Position(6, 0), Position(6, 0)),
                "\t",
            ),
        ],
    )

    await test_case(
        "Indentation case declare block",
        Range(Position(0, 0), Position(2, 0)),
        [
            TextEdit(
                Range(Position(0, 0), Position(0, 0)),
                "",
            ),
            TextEdit(
                Range(Position(1, 0), Position(1, 0)),
                "\t",
            ),
            TextEdit(
                Range(Position(2, 0), Position(2, 0)),
                "\t",
            ),
        ],
    )

    if len(failed_tests) > 0:
        fail_messages = "\n\n".join(failed_tests)
        message = f"Indentation tests failed\n\n{fail_messages}"
        raise Exception(message)  # pylint: disable=broad-exception-raised
