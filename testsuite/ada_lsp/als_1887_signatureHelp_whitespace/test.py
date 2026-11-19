"""
Tests signatureHelp location on all the character location in "Bar (123 , 456)"
The goal is to test which token including whitespace and comma are part of
which parameter.
"""

import os

from drivers.pylsp import URI, ALSLanguageClient, test, Pos
from lsprotocol.types import (
    ClientCapabilities,
    InitializedParams,
    InitializeParams,
    SignatureHelpParams,
    TextDocumentIdentifier,
)


@test(initialize=False)
async def test_help_active_parameter(lsp: ALSLanguageClient) -> None:

    await lsp.initialize_session(
        InitializeParams(ClientCapabilities(), root_uri=URI(os.getcwd()))
    )

    lsp.initialized(InitializedParams())
    lsp.didChangeConfig({"projectFile": URI("test.gpr")})

    main_adb = lsp.didOpenFile("foo.adb")

    main_adb_textdoc_id = TextDocumentIdentifier(main_adb)

    async def test_pos(position, expected):
        res = await lsp.text_document_signature_help_async(
            SignatureHelpParams(main_adb_textdoc_id, position)
        )
        assert res is not None and res.active_parameter == expected

    await test_pos(Pos(4, 10), 0)  # '1'
    await test_pos(Pos(4, 11), 0)  # '2'
    await test_pos(Pos(4, 12), 0)  # '3'
    await test_pos(Pos(4, 13), 0)  # ' '
    await test_pos(Pos(4, 14), 1)  # ','
    await test_pos(Pos(4, 15), 1)  # ' '
    await test_pos(Pos(4, 16), 1)  # '4'
    await test_pos(Pos(4, 17), 1)  # '5'
    await test_pos(Pos(4, 18), 1)  # '6'
