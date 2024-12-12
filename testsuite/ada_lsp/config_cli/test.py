"""
The goal of this test is to check that when --config CONFIG_FILE is given, that
configuration is loaded.

To test that we create a unique p1.gpr at the root. If there was no configuration,
p1.gpr would be loaded automatically. It's a failure sentinel.

Then we create a my_config.json file that sets projectFile="non-root/p2.gpr". If this
file is loaded, then p2.gpr is our success sentinel.

Then we start the ALS with --config my_config.json and query for the current project to
determine the success or failure.
"""

import os

from drivers.pylsp import (
    URI,
    ALSClientServerConfig,
    ALSLanguageClient,
    assertEqual,
    test,
)


@test(
    config=ALSClientServerConfig(
        [os.environ.get("ALS", "ada_language_server"), "--config", "my_config.json"]
    )
)
async def test_func(lsp: ALSLanguageClient) -> None:
    response = await lsp.getCurrentProject()
    assert response
    assertEqual(response, URI("non-root/p2.gpr"))
