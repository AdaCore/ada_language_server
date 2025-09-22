"""Test the rangeFormatting with tabs when there are no diagnostics"""

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
    lsp.didChangeConfig({"rangeFormattingFallback": False})
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

    # Range formatting using GNATformat is trying to reformat most of the
    # procedure everytime so let do a full format once and not individual
    # test cases
    await test_case(
        "Indentation full file",
        Range(Position(1, 0), Position(29, 0)),
        [
            TextEdit(
                Range(Position(1, 0), Position(3, 0)),
                "\tX   : Integer := 1;\n\tYYY : Natural := 2;\n",
            ),
            TextEdit(
                Range(Position(4, 0), Position(7, 0)),
                "\tif X = YYY or else X + X > YYY and then"
                + " (X < YYY and then Y > 2) then\n\t\tnull;\n\tend if;\n",
            ),
            TextEdit(
                Range(Position(8, 0), Position(13, 0)),
                "\tfor I in 1 .. X loop\n\t\tif X > 2 then\n\t\t\t"
                + "Y := Y + 1;\n\t\tend if;\n\tend loop;\n",
            ),
            TextEdit(
                Range(Position(14, 0), Position(17, 0)),
                "\tcase X is\n\t\twhen X > 10          =>\n\t\t\tnull;\n",
            ),
            TextEdit(
                Range(Position(18, 0), Position(20, 0)),
                "\t\twhen X > 20          =>\n\t\t\tnull;\n",
            ),
            TextEdit(
                Range(Position(21, 0), Position(23, 0)),
                "\t\twhen X > 30 | Y > 10 =>\n\t\t\tnull;\n",
            ),
            TextEdit(
                Range(Position(24, 0), Position(27, 0)),
                "\t\twhen others          =>\n\t\t\tnull;\n\tend case;\n",
            ),
        ],
    )

    if len(failed_tests) > 0:
        fail_messages = "\n\n".join(failed_tests)
        message = f"Indentation tests failed\n\n{fail_messages}"
        raise Exception(message)  # pylint: disable=broad-exception-raised
