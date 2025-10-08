"""Test the onTypeFormatting with indentSize when there are no diagnostics"""

import os
from drivers.pylsp import URI, ALSLanguageClient, OnTypeFormattingSetting, test
from drivers.lsp_ada_requests import run_indentation_testcases, IndentationTestCase
from lsprotocol.types import (
    ClientCapabilities,
    DocumentOnTypeFormattingOptions,
    FormattingOptions,
    InitializedParams,
    InitializeParams,
    Position,
)


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
            "Indentation after when",
            "t.adb",
            Position(15, 29),
            "               ",
        ),
        IndentationTestCase(
            "Indentation after case",
            "t.adb",
            Position(14, 12),
            "          ",
        ),
        IndentationTestCase(
            "Indentation after for+if/then",
            "t.adb",
            Position(9, 19),
            "               ",
        ),
        IndentationTestCase(
            "Indentation after for",
            "t.adb",
            Position(8, 23),
            "          ",
        ),
        IndentationTestCase(
            "Indentation after end if",
            "t.adb",
            Position(6, 10),
            "     ",
        ),
        IndentationTestCase(
            "Indentation after if/then",
            "t.adb",
            Position(4, 72),
            "          ",
        ),
    ]

    failed_tests = await run_indentation_testcases(
        lsp, indentation_tests, FormattingOptions(tab_size=5, insert_spaces=True)
    )

    if len(failed_tests) > 0:
        fail_messages = "\n\n".join(failed_tests)
        message = f"Indentation tests failed\n\n{fail_messages}"
        raise Exception(message)  # pylint: disable=broad-exception-raised
