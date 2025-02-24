import os

from drivers.pylsp import URI, ALSLanguageClient, test, Pos
from lsprotocol.types import (
    ClientCapabilities,
    DidChangeTextDocumentParams,
    InitializedParams,
    InitializeParams,
    Position,
    Range,
    SignatureHelpContext,
    SignatureHelpParams,
    SignatureHelpTriggerKind,
    TextDocumentContentChangeEvent_Type1,
    TextDocumentIdentifier,
    VersionedTextDocumentIdentifier,
)


# Use initialize=False so we can inspect the results of the initialize request
@test(initialize=False)
async def test_help_active_parameter_multiple_signature(lsp: ALSLanguageClient) -> None:

    # Helper function to modify the document
    def text_document_did_change(
        version: int, uri: str, posBeg: Position, posEnd: Position, text: str
    ):
        lsp.text_document_did_change(
            DidChangeTextDocumentParams(
                VersionedTextDocumentIdentifier(version, uri),
                [TextDocumentContentChangeEvent_Type1(Range(posBeg, posEnd), text)],
            )
        )

    # Send the initialize request
    await lsp.initialize_session(
        InitializeParams(ClientCapabilities(), root_uri=URI(os.getcwd()))
    )

    # Send the initialized notification and the didChangeConfiguration notification
    lsp.initialized(InitializedParams())
    lsp.didChangeConfig({"projectFile": URI("default.gpr")})

    # Send a didOpen for main.adb
    main_adb = lsp.didOpenFile("foo.adb")

    # Create the TextDocumentIdentifier
    main_adb_textdoc_id = TextDocumentIdentifier(main_adb)

    # Send a textDocument/signatureHelp request
    res = await lsp.text_document_signature_help_async(
        SignatureHelpParams(TextDocumentIdentifier(main_adb), Pos(20, 8))
    )
    assert res is not None and res.active_parameter == 0
    assert res.active_signature == 0
    assert len(res.signatures) == 2

    # Send a textDocument/didChange request
    text_document_did_change(1, main_adb, Pos(20, 9), Pos(20, 10), "1")

    res.active_signature = 1
    res = await lsp.text_document_signature_help_async(
        SignatureHelpParams(
            main_adb_textdoc_id,
            Pos(20, 10),
            SignatureHelpContext(SignatureHelpTriggerKind(3), True, "(", res),
        )
    )
    assert res is not None and res.active_parameter == 0
    assert res.active_signature == 1
    assert len(res.signatures) == 2

    text_document_did_change(2, main_adb, Pos(20, 11), Pos(20, 13), ",5,")

    res.active_signature = 1
    res = await lsp.text_document_signature_help_async(
        SignatureHelpParams(
            main_adb_textdoc_id,
            Pos(20, 13),
            SignatureHelpContext(SignatureHelpTriggerKind(3), True, "(", res),
        )
    )
    assert res is not None and res.active_parameter == 2
    assert res.active_signature == 1
    assert len(res.signatures) == 2

    text_document_did_change(3, main_adb, Pos(20, 14), Pos(20, 15), "6,")

    res = await lsp.text_document_signature_help_async(
        SignatureHelpParams(
            main_adb_textdoc_id,
            Pos(20, 15),
        )
    )
    assert res is not None and res.active_parameter == 3
    assert res.active_signature == 0
    assert len(res.signatures) == 1
