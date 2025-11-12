"""Test the textDocument/formatting request for GPR files"""

import os

from drivers.pylsp import URI, ALSClientServerConfig, ALSLanguageClient, test
from lsprotocol.types import (
    ClientCapabilities,
    DocumentFormattingParams,
    FormattingOptions,
    InitializedParams,
    InitializeParams,
    Position,
    Range,
    TextDocumentIdentifier,
    TextEdit,
    VersionedTextDocumentIdentifier,
)


expected_text_edits: list[TextEdit] = [
    TextEdit(Range(Position(0, 0), Position(0, 3)), new_text=""),
    TextEdit(Range(Position(2, 0), Position(2, 3)), new_text="   "),
    TextEdit(Range(Position(3, 0), Position(3, 6)), new_text="      "),
    TextEdit(
        Range(Position(4, 0), Position(4, 7)),
        new_text="                                        ",
    ),
    TextEdit(Range(Position(5, 0), Position(5, 5)), new_text="   "),
    TextEdit(Range(Position(7, 0), Position(7, 2)), new_text=""),
]


@test(
    config=ALSClientServerConfig(
        [
            os.environ.get("ALS", "ada_language_server"),
            "--language-gpr",
        ]
    ),
)
async def test_formatting(lsp: ALSLanguageClient) -> None:

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

    # Test formatting for both files: one with newline at the end of
    # it and one without
    for filename in ["test.gpr", "test_no_newline.gpr"]:
        source = VersionedTextDocumentIdentifier(
            uri=lsp.didOpenFile(
                os.path.join(os.getcwd(), filename), language_id="Project File"
            ),
            version=0,
        )

        text_edits = await lsp.text_document_formatting_async(
            DocumentFormattingParams(
                TextDocumentIdentifier(source.uri),
                FormattingOptions(tab_size=3, insert_spaces=False),
            )
        )
        lsp.assertEqual(text_edits, expected_text_edits)
