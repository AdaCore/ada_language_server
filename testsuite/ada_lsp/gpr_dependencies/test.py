import os
from typing import Any

from drivers.pylsp import URI, ALSLanguageClient, test

from lsprotocol.types import (
    ClientCapabilities,
    InitializedParams,
    InitializeParams,
    ExecuteCommandParams,
)
from enum import Enum


class ALS_GprDependencyDirection(Enum):
    SHOW_DEPENDENT = 1
    SHOW_DEPENDING = 2


class ALS_GprDependencyKind(Enum):
    AGGREGATED = 0
    EXTENDED = 1
    EXTENDING = 2
    IMPORTED = 3


# Use initialize=False so we can inspect the results of the initialize request
@test(initialize=False)
async def test_gpr_dependencies(lsp: ALSLanguageClient) -> None:

    # Helper function to send als_gpr_dependencies request.
    async def send_gpr_dependencies(
        gprPath: str, direction: ALS_GprDependencyDirection
    ):
        dependencies: list[dict[str, Any]] | None = (
            await lsp.workspace_execute_command_async(
                ExecuteCommandParams(
                    "als-gpr-dependencies",
                    [
                        {
                            "uri": URI(gprPath),
                            "direction": direction.value,
                        },
                    ],
                ),
            )
        )
        return dependencies

    # Send the initialize request
    serverCapabilities = await lsp.initialize_session(
        InitializeParams(ClientCapabilities(), root_uri=URI(os.getcwd()))
    )

    assert serverCapabilities.capabilities.execute_command_provider is not None
    assert (
        "als-gpr-dependencies"
        in serverCapabilities.capabilities.execute_command_provider.commands
    )

    # Send the initialized notification and the didChangeConfiguration notification
    lsp.initialized(InitializedParams())
    lsp.didChangeConfig({"projectFile": URI("default.gpr")})

    base_path = URI(os.getcwd())

    #####################################################################################

    dependencies = await send_gpr_dependencies(
        "./default.gpr", ALS_GprDependencyDirection.SHOW_DEPENDENT
    )

    lsp.assertEqual(
        dependencies,
        [{"uri": base_path + "/subProject/subProject.gpr", "kind": 3}],
    )

    #####################################################################################

    dependencies = await send_gpr_dependencies(
        "./default.gpr", ALS_GprDependencyDirection.SHOW_DEPENDING
    )

    lsp.assertEqual(dependencies, [])

    #####################################################################################

    dependencies = await send_gpr_dependencies(
        "./subProject/subProject.gpr", ALS_GprDependencyDirection.SHOW_DEPENDENT
    )

    lsp.assertEqual(
        dependencies,
        [
            {
                "uri": base_path + "/extendedProject/extendedProject.gpr",
                "kind": 1,
            }
        ],
    )

    #####################################################################################

    dependencies = await send_gpr_dependencies(
        "./subProject/subProject.gpr", ALS_GprDependencyDirection.SHOW_DEPENDING
    )

    lsp.assertEqual(dependencies, [{"uri": base_path + "/default.gpr", "kind": 3}])

    #####################################################################################

    dependencies = await send_gpr_dependencies(
        "./extendedProject/extendedProject.gpr",
        ALS_GprDependencyDirection.SHOW_DEPENDENT,
    )

    lsp.assertEqual(dependencies, [])

    #####################################################################################

    dependencies = await send_gpr_dependencies(
        "./extendedProject/extendedProject.gpr",
        ALS_GprDependencyDirection.SHOW_DEPENDING,
    )

    lsp.assertEqual(
        dependencies,
        [{"uri": base_path + "/subProject/subProject.gpr", "kind": 1}],
    )

    #####################################################################################
