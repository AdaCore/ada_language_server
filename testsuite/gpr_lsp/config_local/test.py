"""
The goal of this test is to check that when a .als.json file exists at the root of
the workspace, the GPR language server loads it before loading the project.
This is done through checking that no diagnostics are reported about the undefined
BUILD_MODDE external variable (which is precisely defined in the .als.json file)
when opening a GPR file.
The second test checks that sending empty initializationOptions does not overwrite the
scenarioVariables loaded from .als.json.
"""

import os

from drivers.pylsp import ALSClientServerConfig, ALSLanguageClient, test


def gpr_config() -> ALSClientServerConfig:
    return ALSClientServerConfig(
        [os.environ.get("ALS", "ada_language_server"), "--language-gpr"]
    )


@test(config=gpr_config())
async def func(lsp: ALSLanguageClient) -> None:
    uri = lsp.didOpenFile("prj.gpr", language_id="Gpr")
    await lsp.sleep(1.0)
    diags = lsp.diagnostics.get(uri, [])
    assert not diags, f"Expected no diagnostics, got: {diags}"


@test(config=gpr_config(), als_settings={})
async def test_init_override(lsp: ALSLanguageClient) -> None:
    # Check that sending empty initializationOptions does not overwrite the
    # scenarioVariables loaded from .als.json.
    uri = lsp.didOpenFile("prj.gpr", language_id="Gpr")
    await lsp.sleep(1.0)
    diags = lsp.diagnostics.get(uri, [])
    assert (
        not diags
    ), f"Expected no diagnostics after empty initializationOptions, got: {diags}"
