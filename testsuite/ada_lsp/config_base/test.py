"""
The goal of these tests is to check that ALS remembers a base configuration to which it
can revert settings when receiving null values in didChangeConfiguration notifiactions.

It's important to check that the base configuration can be obtained both when there are
config files (.als.json or User config file), and when there aren't - i.e. when settings
are sent with the initialize request, or with the first didChangeConfiguration
notification.

To observe the active configuration, we use a project file where the object directory is
defined via an external scenario variable and we use the als-object-dir command to query
that.

We also need a way to make sure the project has been loaded in order to query its object
dir. To do that we add at least one source file and use didOpen on it.

The scenarios we want to test:

1. There is a config file. Then the initialize request also provides settings, but the
base config should be the config file.

2. There are no config files, and the initialize request contains settings with non-null
values that should be the base config.

3. There are no config files, and the initialize request contains settings with all null
values. This is the case when no ada.* settings are set in VS Code. The config at
initialize should be considered the base config.

4. There are no config files. The initialize request doesn't contain any settings. The
first didChangeConfig notification should be considered as the base config.
"""

import os
from lsprotocol.types import ClientCapabilities, InitializeParams
from drivers.pylsp import (
    URI,
    ALSClientServerConfig,
    ALSLanguageClient,
    ALSSettings,
    assertEqual,
    test,
)


@test(
    config=ALSClientServerConfig(
        server_command=[
            os.environ.get("ALS", "ada_language_server"),
            "--config",
            "my_config.json",
        ],
    ),
    initialize=False,
)
async def test1(lsp: ALSLanguageClient) -> None:
    # Even if we send settings in the initialize request, the config file should
    # constitue the base config.
    settings: ALSSettings = {
        "scenarioVariables": {"Var": "value-from-init"},
    }
    await lsp.initialize_session(
        InitializeParams(
            ClientCapabilities(),
            root_uri=URI(os.getcwd()),
            initialization_options={"ada": settings},
        )
    )
    # Because no project file was set, we need a didOpen to load the project
    lsp.didOpenVirtual()
    assertEqual(await lsp.getObjDirBasename(), "value-from-init")

    # Now let's change the settings
    lsp.didChangeConfig({"scenarioVariables": {"Var": "new-value"}})
    assertEqual(await lsp.getObjDirBasename(), "new-value")

    # Now we send a null value to revert to the base config which should be the config
    # file, not the initialize request.
    lsp.didChangeConfig({"scenarioVariables": None})
    assertEqual(await lsp.getObjDirBasename(), "value-from-config-file")


@test(initialize=False)
async def test2(lsp: ALSLanguageClient) -> None:
    # In this scenario there are no config files. The settings in the initialize request
    # should constitute the base config.
    settings: ALSSettings = {
        "scenarioVariables": {"Var": "value-from-init"},
    }
    await lsp.initialize_session(
        InitializeParams(
            ClientCapabilities(),
            root_uri=URI(os.getcwd()),
            initialization_options={"ada": settings},
        )
    )
    # Because no project file was set, we need a didOpen to load the project
    lsp.didOpenVirtual()
    assertEqual(await lsp.getObjDirBasename(), "value-from-init")

    # Now let's change the settings and revert back to see if we revert to the right
    # value.
    lsp.didChangeConfig({"scenarioVariables": {"Var": "new-value"}})
    assertEqual(await lsp.getObjDirBasename(), "new-value")

    lsp.didChangeConfig({"scenarioVariables": None})
    assertEqual(await lsp.getObjDirBasename(), "value-from-init")


@test(initialize=False)
async def test3(lsp: ALSLanguageClient) -> None:
    # This scenario reproduces what happens in VS Code when there are no ada.* settings
    #
    # The settings in the initialize request are null
    settings: ALSSettings = {
        "scenarioVariables": None,
    }
    await lsp.initialize_session(
        InitializeParams(
            ClientCapabilities(),
            root_uri=URI(os.getcwd()),
            initialization_options={"ada": settings},
        )
    )
    # Because no project file was set, we need a didOpen to load the project
    lsp.didOpenVirtual()
    # No value was provided for the scenario variable, so we should get the default
    # value defined in the project.
    assertEqual(await lsp.getObjDirBasename(), "value-from-prj")

    # Now let's change the settings and revert back to see if we revert to the right
    # value.
    lsp.didChangeConfig({"scenarioVariables": {"Var": "new-value"}})
    assertEqual(await lsp.getObjDirBasename(), "new-value")

    lsp.didChangeConfig({"scenarioVariables": None})
    assertEqual(await lsp.getObjDirBasename(), "value-from-prj")


@test(initialize=False)
async def test4(lsp: ALSLanguageClient) -> None:
    # There is no config file
    await lsp.initialize_session(
        InitializeParams(
            ClientCapabilities(),
            root_uri=URI(os.getcwd()),
            # initialize has no settings
        )
    )
    # Because no project file was set, we need a didOpen to load the project
    lsp.didOpenVirtual()
    # No value was provided for the scenario variable, so we should get the default
    # value defined in the project.
    assertEqual(await lsp.getObjDirBasename(), "value-from-prj")

    # Now let's send the first didChangeConfiguration. This should constitute the base
    # config.
    lsp.didChangeConfig(
        {"scenarioVariables": {"Var": "value-from-first-config-change"}}
    )
    assertEqual(await lsp.getObjDirBasename(), "value-from-first-config-change")

    # Now we change to another value, and revert with a null value.
    lsp.didChangeConfig({"scenarioVariables": {"Var": "new-value"}})
    assertEqual(await lsp.getObjDirBasename(), "new-value")

    lsp.didChangeConfig({"scenarioVariables": None})
    assertEqual(await lsp.getObjDirBasename(), "value-from-first-config-change")
