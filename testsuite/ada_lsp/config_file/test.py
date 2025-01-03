"""
The goal of this test is to check that we take in account
configurationFile configuration option and have proper navigation
"""

import subprocess
from drivers.pylsp import (
    ALSLanguageClient,
    URI,
    didOpenTextDocumentParams,
    assertEqual,
    test,
)
from lsprotocol.types import (
    DeclarationParams,
    TextDocumentIdentifier,
    Position,
    Location,
)


# create default.cgpr configuration file that will use not-default runtime
subprocess.check_call(["gprconfig", "--batch", "--config=Ada,,light,,GNAT"])


@test()
async def do_testing(lsp: ALSLanguageClient) -> None:
    # Set configuration file
    lsp.didChangeConfig(
        {"projectFile": URI("main.gpr"), "gprConfigurationFile": URI("default.cgpr")}
    )

    # Send a didOpen for main.adb
    open_params, main_adb_uri = didOpenTextDocumentParams("main.adb")
    lsp.text_document_did_open(open_params)

    # Send `goto declaration` request
    result = await lsp.text_document_declaration_async(
        DeclarationParams(TextDocumentIdentifier(main_adb_uri), Position(4, 48))
    )

    # Check result
    assert result
    assert isinstance(result, Location)
    assertEqual("light" in result.uri, True)

    # Check relative to root paths
    lsp.didChangeConfig(
        {"projectFile": "main1.gpr", "gprConfigurationFile": "default.cgpr"}
    )

    # Send a didOpen for main.adb
    open_params, main_adb_uri = didOpenTextDocumentParams("main.adb")
    lsp.text_document_did_open(open_params)

    # Send `goto declaration` request
    result = await lsp.text_document_declaration_async(
        DeclarationParams(TextDocumentIdentifier(main_adb_uri), Position(4, 48))
    )

    # Check result
    assert result
    assert isinstance(result, Location)
    assertEqual("light" in result.uri, True)
