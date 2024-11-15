"""
The goal of this test is to check that when a global $XDG_CONFIG_HOME/als/config.json
config file exists, it is automatically loaded.

To test that we create a unique p1.gpr at the root. If there was no global config file,
p1.gpr would be loaded automatically. It's a failure sentinel.

Then we create a xdg_config_home/als/config.json file that sets
projectFile="non-root/p2.gpr". If that file is loaded, then p2.gpr is our success
sentinel.

Then we start the LSP with the env variable XSD_CONFIG_HOME = "./xdg_config_home" and
query for the current project to determine the success or failure.
"""

import os

from drivers.pylsp import (
    URI,
    ALSClientServerConfig,
    LanguageClient,
    assertEqual,
    getCurrentProject,
    test,
)


@test(
    config=ALSClientServerConfig(
        [os.environ.get("ALS", "ada_language_server")],
        server_env=os.environ | {"XDG_CONFIG_HOME": os.path.abspath("xdg_config_home")},
    )
)
async def func(lsp: LanguageClient) -> None:
    response = await lsp.workspace_execute_command_async(getCurrentProject())
    assert response
    assertEqual(response, URI("non-root/p2.gpr"))
