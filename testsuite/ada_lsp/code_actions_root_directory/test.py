"""Project code actions accept URI, path, and inferred workspace roots."""

from pathlib import Path

from drivers.pylsp import ALSLanguageClient, URI, test
from lsprotocol.types import (
    ClientCapabilities,
    CodeAction,
    CodeActionContext,
    CodeActionParams,
    CreateFile,
    InitializeParams,
    OptionalVersionedTextDocumentIdentifier,
    Position,
    Range,
    TextDocumentEdit,
    TextDocumentIdentifier,
    TextEdit,
    WorkspaceEdit,
)


async def check_code_actions(lsp: ALSLanguageClient, params: InitializeParams):
    await lsp.initialize_session(params)
    uri = lsp.didOpenFile("main.adb")
    start = Range(Position(0, 0), Position(0, 0))
    result = await lsp.text_document_code_action_async(
        CodeActionParams(TextDocumentIdentifier(uri), start, CodeActionContext([]))
    )

    assert result is not None
    actions = [
        action
        for action in result
        if isinstance(action, CodeAction)
        and action.title == "Create a default project file (default.gpr)"
    ]
    lsp.assertEqual(len(actions), 1)
    default_uri = URI(Path.cwd() / "default.gpr")
    lsp.assertEqual(
        actions[0].edit,
        WorkspaceEdit(
            document_changes=[
                CreateFile(uri=default_uri),
                TextDocumentEdit(
                    OptionalVersionedTextDocumentIdentifier(default_uri, None),
                    [TextEdit(start, "project Default is end Default;")],
                ),
            ]
        ),
    )


@test(initialize=False)
async def root_uri(lsp: ALSLanguageClient):
    await check_code_actions(
        lsp, InitializeParams(ClientCapabilities(), root_uri=URI(Path.cwd()))
    )


@test(initialize=False)
async def root_path(lsp: ALSLanguageClient):
    await check_code_actions(
        lsp, InitializeParams(ClientCapabilities(), root_path=str(Path.cwd()))
    )


@test(initialize=False)
async def inferred_root(lsp: ALSLanguageClient):
    await check_code_actions(lsp, InitializeParams(ClientCapabilities()))
