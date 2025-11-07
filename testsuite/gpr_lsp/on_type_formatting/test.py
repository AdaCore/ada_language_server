"""Test the textDocument/onTypeFormatting request on GPR files"""

import os
from drivers.pylsp import (
    URI,
    ALSClientServerConfig,
    ALSLanguageClient,
    OnTypeFormattingSetting,
    test,
)
from drivers.lsp_ada_requests import (
    IndentationTestCase,
    run_indentation_testcases,
)
from lsprotocol.types import (
    ClientCapabilities,
    DocumentOnTypeFormattingOptions,
    FormattingOptions,
    InitializedParams,
    InitializeParams,
    Position,
    Range,
    TextEdit,
)


@test(
    config=ALSClientServerConfig(
        [
            os.environ.get("ALS", "ada_language_server"),
            "--language-gpr",
        ]
    ),
)
async def test_on_type_formatting_indentation(lsp: ALSLanguageClient) -> None:
    """
    Test the onTypeFormatting feature on GPR files.

    This test verifies that ALS correctly estimates the indentation that should
    be added after a line break.

    Tests should be added to the indentation_tests list.
    """

    # Send the initialize request

    response = await lsp.initialize_session(
        InitializeParams(
            capabilities=ClientCapabilities(),
            root_uri=URI(os.getcwd()),
            initialization_options={
                "ada": {"onTypeFormatting": OnTypeFormattingSetting(indentOnly=False)}
            },
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

    # Test list
    indentation_tests = [
        IndentationTestCase(
            "Indentation after for inside package",
            "test.gpr",
            Position(3, 95),
            "      ",
            [TextEdit(Range(Position(3, 0), Position(3, 6)), new_text="      ")],
        ),
        IndentationTestCase(
            "Indentation after end of package",
            "test.gpr",
            Position(4, 18),
            "   ",
            [TextEdit(Range(Position(4, 0), Position(4, 6)), new_text="   ")],
        ),
    ]

    failed_tests = await run_indentation_testcases(
        lsp, indentation_tests, FormattingOptions(tab_size=3, insert_spaces=False)
    )

    if len(failed_tests) > 0:
        fail_messages = "\n\n".join(failed_tests)
        message = f"Indentation tests failed\n\n{fail_messages}"
        raise Exception(message)  # pylint: disable=broad-exception-raised
