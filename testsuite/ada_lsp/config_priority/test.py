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
async def test_global_local(lsp: LanguageClient) -> None:
    """Test that the local .als.json takes priority over the global config file"""
    response = await lsp.workspace_execute_command_async(getCurrentProject())
    assertEqual(response, URI("non-root/p4.gpr"))


@test(
    config=ALSClientServerConfig(
        [os.environ.get("ALS", "ada_language_server"), "--config", "my_config.json"],
        server_env=os.environ | {"XDG_CONFIG_HOME": "some_non_existing_path"},
    )
)
async def test_local_cli(lsp: LanguageClient) -> None:
    """Test that the CLI config file takes priority over the local .als.json one"""
    response = await lsp.workspace_execute_command_async(getCurrentProject())
    assertEqual(response, URI("non-root/p2.gpr"))


@test(
    config=ALSClientServerConfig(
        [os.environ.get("ALS", "ada_language_server"), "--config", "my_config.json"],
        server_env=os.environ | {"XDG_CONFIG_HOME": os.path.abspath("xdg_config_home")},
    )
)
async def test_all(lsp: LanguageClient) -> None:
    """Test that the CLI config file takes priority over both"""
    response = await lsp.workspace_execute_command_async(getCurrentProject())
    assertEqual(response, URI("non-root/p2.gpr"))
