"""Test the onTypeFormatting feature of ALS"""

import os
from typing import List

from attr import dataclass
from drivers.pylsp import URI, ALSLanguageClient, OnTypeFormattingSetting, test
from lsprotocol.types import (
    ClientCapabilities,
    DidChangeTextDocumentParams,
    DocumentOnTypeFormattingOptions,
    DocumentOnTypeFormattingParams,
    FormattingOptions,
    InitializedParams,
    InitializeParams,
    Position,
    Range,
    TextDocumentContentChangeEvent_Type1,
    TextDocumentIdentifier,
    TextEdit,
    VersionedTextDocumentIdentifier,
)


@dataclass
class IndentationTestCase:
    """
    IndentationTestCase is a class that represents a test case for indentation
    handling in onTypeFormatting requests. It contains the following
    attributes:

    Attributes:
        description (str): A brief description of the test case.
        source_filename (str): The filename of the source code to be tested.
            line_break_position (Position): The position in the source code
            where the line break occurs.
        expected_indentation (str): The expected indentation string after the
            onTypeFormatting request is applied.
    """

    description: str
    source_filename: str
    line_break_position: Position
    expected_indentation: str

    @property
    def change_range(self) -> Range:
        """
        Returns a Range object representing the range of the line break
        position. The range is defined by the start and end positions, both set
        to the line break position of the current instance.
        """
        return Range(self.line_break_position, self.line_break_position)

    @property
    def on_type_formatting_request_position(self) -> Position:
        """
        Determines the position for onTypeFormatting request.

        This method calculates the position where the onTypeFormatting should
        be requested. It returns a new position that is one line below the
        current line break position and at the start of the line (column 0).
        """
        return Position(self.line_break_position.line + 1, 0)

    @property
    def expected_text_edits(self) -> List[TextEdit]:
        """
        Generates a expected reponse (list of TextEdit objects) for the
        onTypeFormatting request.
        """
        return [
            TextEdit(
                Range(
                    Position(self.line_break_position.line + 1, 0),
                    Position(self.line_break_position.line + 1, 0),
                ),
                self.expected_indentation,
            )
        ]


@test(initialize=False)
async def test_on_type_formatting_indentation(lsp: ALSLanguageClient) -> None:
    """
    Test the onTypeFormatting feature of ALS.

    This test verifies that ALS correctly estimates the indentation that should
    be added after a line break.

    Tests should be added to the indentation_tests list.
    """

    # Send the initialize request

    response = await lsp.initialize_session(
        InitializeParams(
            capabilities=ClientCapabilities(),
            root_uri=URI(os.getcwd()),
        )
    )

    # Verify that the right capability is advertised

    lsp.assertEqual(
        response.capabilities.document_on_type_formatting_provider,
        DocumentOnTypeFormattingOptions(
            first_trigger_character="\n", more_trigger_character=None
        ),
    )

    # Send the initialized notification and the didChangeConfiguration
    # notification, configuring ALS to only indent when it receives an
    # onTypeFormattingRequest

    lsp.initialized(InitializedParams())

    lsp.didChangeConfig({"onTypeFormatting": OnTypeFormattingSetting(indentOnly=True)})
    await lsp.awaitIndexingEnd()

    # Test list
    indentation_tests = [
        IndentationTestCase(
            "Indentation after an association in an association list",
            "after_association.adb",
            Position(4, 19),
            "        ",
        ),
        IndentationTestCase(
            "Indentation after the begin keyword",
            "after_begin.adb",
            Position(2, 8),
            "      ",
        ),
        IndentationTestCase(
            "Indentation after the declare keyword",
            "after_declare.adb",
            Position(2, 10),
            "      ",
        ),
        IndentationTestCase(
            "Indentation after dot in a dotted name",
            "after_dot.adb",
            Position(2, 7),
            "     ",
        ),
        IndentationTestCase(
            "Indentation after a generic formal declaration",
            "after_generic_formal_decl.ads",
            Position(0, 7),
            "   ",
        ),
        IndentationTestCase(
            "Indentation after the is keyword of a generic package",
            "after_generic_package_is.ads",
            Position(1, 35),
            "   ",
        ),
        IndentationTestCase(
            "Indentation after the private keyword of a generic package",
            "after_generic_package_private.ads",
            Position(2, 7),
            "   ",
        ),
        IndentationTestCase(
            "Indentation after an if statement",
            "after_if_stmt.adb",
            Position(4, 10),
            "   ",
        ),
        IndentationTestCase(
            "Indentation after the begin keyword of a package body",
            "after_package_body_begin.adb",
            Position(1, 5),
            "   ",
        ),
        IndentationTestCase(
            "Indentation after the is keyword of a package body",
            "after_package_body_is.adb",
            Position(0, 40),
            "   ",
        ),
        IndentationTestCase(
            "Indentation after a statement in a package body",
            "after_package_body_statement.adb",
            Position(2, 8),
            "   ",
        ),
        IndentationTestCase(
            "Indentation after the is keyword of a package",
            "after_package_is_1.ads",
            Position(0, 29),
            "   ",
        ),
        IndentationTestCase(
            "Indentation after the is keyword of a package on an empty line",
            "after_package_is_2.ads",
            Position(1, 0),
            "   ",
        ),
        IndentationTestCase(
            "Indentation after a whole line comment that is after the is keyword"
            " of a package",
            "after_package_is_3.ads",
            Position(1, 14),
            "   ",
        ),
        IndentationTestCase(
            "Indentation after a package private keyword",
            "after_package_private.ads",
            Position(1, 7),
            "   ",
        ),
        IndentationTestCase(
            "Indentation after the begin keyword of a subprogram body",
            "after_procedure_begin.adb",
            Position(7, 8),
            "      ",
        ),
        IndentationTestCase(
            "Indentation after the is keyword of a subprogram body",
            "after_procedure_is.adb",
            Position(6, 5),
            "      ",
        ),
        IndentationTestCase(
            "Indentation after the subprogram name",
            "after_subp_name.ads",
            Position(1, 16),
            "     ",
        ),
        IndentationTestCase(
            "Indentation after a subprogram parameter",
            "after_subp_param.ads",
            Position(2, 14),
            "      ",
        ),
        IndentationTestCase(
            "Indentation after a subprogram",
            "after_subp.ads",
            Position(3, 15),
            "   ",
        ),
    ]

    failed_tests = []

    # For each test:
    # - Send a didChange notification with a line break
    # - Send a onTypeFormattting request on the new line
    # - Check if the reponse is the expected one

    for indentation_test in indentation_tests:
        try:
            source = VersionedTextDocumentIdentifier(
                uri=lsp.didOpenFile(
                    os.path.join(os.getcwd(), "src", indentation_test.source_filename)
                ),
                version=0,
            )

            lsp.text_document_did_change(
                DidChangeTextDocumentParams(
                    text_document=source,
                    content_changes=[
                        TextDocumentContentChangeEvent_Type1(
                            range=indentation_test.change_range,
                            range_length=0,
                            text="\n",
                        )
                    ],
                )
            )

            text_edits = await lsp.text_document_on_type_formatting_async(
                DocumentOnTypeFormattingParams(
                    text_document=TextDocumentIdentifier(source.uri),
                    position=indentation_test.on_type_formatting_request_position,
                    ch="\n",
                    options=FormattingOptions(tab_size=3, insert_spaces=True),
                )
            )
            lsp.assertEqual(text_edits, indentation_test.expected_text_edits)

        except Exception as e:  # pylint: disable=broad-exception-caught
            failed_tests.append(
                f"Test case '{indentation_test.description}' failed:\n{str(e)}"
            )

    if len(failed_tests) > 0:
        fail_messages = "\n\n".join(failed_tests)
        message = f"Indentation tests failed\n\n{fail_messages}"
        raise Exception(message)  # pylint: disable=broad-exception-raised
