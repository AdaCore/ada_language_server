"""
Verify we are sending showMessages in response of invalid configuration but
not when resetting it using a null value for a setting.
"""

from drivers.pylsp import (
    ALSLanguageClient,
    assertEqual,
    test,
)

EXPECTED = [
    'Unknown Ada setting "unknownAttr".',
    'Ada settings are case sensitive: "USEGNATFORMAT" has been ignored '
    + 'please set it to "useGnatformat".',
    'Invalid type for the Ada setting "logThreshold" please check the value.',
]


@test(
    als_settings={
        # Disable indexing to avoid wasting computation resources and risking test
        # timeouts
        "enableIndexing": False
    },
    timeout=30,
)
async def main(lsp: ALSLanguageClient) -> None:
    # There is no config file
    lsp.didOpenVirtual()

    # Disable mypy warning for the ignore below, it's detecting unknown
    # attribute for didChangeConfig which is the goal of the test
    lsp.didChangeConfig(
        {
            "unknownAttr": "Hello",  # type: ignore
            "USEGNATFORMAT": False,  # type: ignore
            "logThreshold": False,
            "insertWithClauses": True,
            "gprConfigurationFile": None,
        }
    )

    # We want to wait until the configuration change was handled before asserting test
    # results. didChangeConfig is handled in "Fence" priority on the ALS side, so if we
    # send any other request, it will be processed after the configuration change.
    await lsp.getCurrentProject()

    total_log_msg = len(lsp.log_messages)
    total_show_msg = len(lsp.messages)
    # the first logMessage is about the log file location, ignore it
    assertEqual([msg.message for msg in lsp.log_messages[1:]], EXPECTED)
    assertEqual([msg.message for msg in lsp.messages], EXPECTED)

    lsp.didChangeConfig({"logThreshold": None, "insertWithClauses": None})

    # Wait for didChangeConfig to be handled by sending any other request
    await lsp.getCurrentProject()

    # Check that no messages were sent after using None/null as the value for
    # a setting
    assertEqual(len(lsp.log_messages), total_log_msg)
    assertEqual(len(lsp.messages), total_show_msg)
