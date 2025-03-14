"""
The goal of this test is to check that the ALS loads the project early in the session,
such that immediately after the initialization sequence, it is possible to inspect
project attributes without opening any Ada sources or sending a configuration change to
force a project load.
"""

from drivers.pylsp import ALSLanguageClient, test


@test(
    # Sending initializationOptions should cause the ALS to load a project early,
    # when handling the `initialized` notification. So we give an empty settings
    # dictionary to force the sending of initializationOptions.
    als_settings={}
)
async def t1(lsp: ALSLanguageClient):
    # Query a project attribute
    result = await lsp.getProjectAttributeValue("harness_dir", "gnattest")

    lsp.assertEqual(result, "myharness")
