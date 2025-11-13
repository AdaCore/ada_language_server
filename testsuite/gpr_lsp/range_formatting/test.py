"""Test the rangeFormatting request for GPR files"""

import os

from drivers.pylsp import URI, ALSClientServerConfig, ALSLanguageClient, test
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


@test(
    config=ALSClientServerConfig(
        [
            os.environ.get("ALS", "ada_language_server"),
            "--language-gpr",
        ]
    ),
)
async def test_range_formatting(lsp: ALSLanguageClient) -> None:

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
    source = VersionedTextDocumentIdentifier(
        uri=lsp.didOpenFile(
            os.path.join(os.getcwd(), "test.gpr"), language_id="Project File"
        ),
        version=0,
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

    # Test indentation of a range covering the GPR file first line: we should
    # remove the extra spaces at the beginning of the line
    await test_case(
        "rangeFormatting on first line",
        Range(Position(0, 0), Position(0, 5)),
        [
            TextEdit(
                Range(Position(0, 0), Position(0, 3)),
                "",
            ),
        ],
    )

    # Test indentation of a range covering the GPR file last line (empty line):
    # we should do nothing since the line is empty
    await test_case(
        "rangeFormatting on last (empty) line",
        Range(Position(8, 0), Position(8, 1)),
        None,
    )

    # Test indentation of a range covering the whole GPR file.
    await test_case(
        "rangeFormatting on whole file",
        Range(Position(0, 0), Position(8, 1)),
        [
            TextEdit(Range(Position(0, 0), Position(0, 3)), ""),
            TextEdit(Range(Position(2, 0), Position(2, 3)), "   "),
            TextEdit(Range(Position(3, 0), Position(3, 6)), "      "),
            TextEdit(
                Range(Position(4, 0), Position(4, 7)),
                "                                        ",
            ),
            TextEdit(Range(Position(5, 0), Position(5, 5)), "   "),
            TextEdit(Range(Position(7, 0), Position(7, 2)), ""),
        ],
    )

    if len(failed_tests) > 0:
        fail_messages = "\n\n".join(failed_tests)
        message = f"Indentation tests failed\n\n{fail_messages}"
        raise Exception(message)  # pylint: disable=broad-exception-raised
