import os
from pathlib import Path

from drivers import pylsp
from lsprotocol.types import (
    ClientCapabilities,
    CompletionClientCapabilities,
    CompletionClientCapabilitiesCompletionItemType,
    CompletionList,
    CompletionParams,
    InitializeParams,
    InsertTextFormat,
    InsertTextMode,
    Position,
    Range,
    TextDocumentClientCapabilities,
    TextDocumentIdentifier,
    TextEdit,
)

CONFIG = pylsp.ALSClientServerConfig(
    [os.environ.get("ALS", "ada_language_server"), "--tracefile=traces.cfg"]
)


async def check_completions(
    lsp: pylsp.ALSLanguageClient,
    snippet_support: bool = True,
    use_snippets: bool = True,
    threshold: int = 3,
) -> None:
    await lsp.initialize_session(
        InitializeParams(
            capabilities=ClientCapabilities(
                text_document=TextDocumentClientCapabilities(
                    completion=CompletionClientCapabilities(
                        completion_item=CompletionClientCapabilitiesCompletionItemType(
                            snippet_support=snippet_support
                        )
                    )
                )
            ),
            root_uri=pylsp.URI("."),
            initialization_options={
                "ada": {
                    "useCompletionSnippets": use_snippets,
                    "namedNotationThreshold": threshold,
                    "enableIndexing": False,
                }
            },
        )
    )
    uri = lsp.didOpenFile("main.adb")
    lines = Path("main.adb").read_text().splitlines()
    # Source, label, replacement column/length, and expected placeholder count.
    cases = [
        ("   Long_N;", "Long_Name", 3, 6, 8),
        ("   Small_N;", "Small_Name", 3, 7, 2),
        ("   No_Ar;", "No_Args", 3, 5, 0),
        ("   Val;", "Value", 3, 3, 0),
        ("   Ops.Long_N;", "Long_Name", 7, 6, 8),
        ("   Ops.;", "Long_Name", 7, 0, 8),
        ("   Obj.Long_M;", "Long_Method", 7, 6, 8),
        ("   Obj.;", "Long_Method", 7, 0, 8),
    ]
    for source, label, column, length, count in cases:
        line = lines.index(source)
        result = await lsp.text_document_completion_async(
            CompletionParams(
                TextDocumentIdentifier(uri), Position(line, column + length)
            )
        )
        assert isinstance(result, CompletionList)
        item = next(item for item in result.items if item.label == label)

        if not (snippet_support and use_snippets and count):
            assert item.text_edit is None
            assert item.insert_text_format != InsertTextFormat.Snippet
            continue

        assert item.insert_text_format == InsertTextFormat.Snippet
        assert item.insert_text_mode == InsertTextMode.AsIs
        assert isinstance(item.text_edit, TextEdit)
        lsp.assertEqual(
            item.text_edit.range,
            Range(Position(line, column), Position(line, column + length)),
        )
        text = item.text_edit.new_text
        assert text.startswith(label)
        assert text.endswith(")$0")
        # Restore the original hints after formatting with fake identifiers.
        assert "foobar_" not in text.lower()
        assert text.count("${") == count
        assert "out Integer}" in text
        assert "in out Integer}" in text
        if count == 8:
            assert "\n" in text
        if threshold == 0 or count == 2:
            assert " => " not in text
        else:
            assert "First_Long_Parameter" in text
            assert " => " in text


@pylsp.test(config=CONFIG, initialize=False)
async def named(lsp: pylsp.ALSLanguageClient) -> None:
    await check_completions(lsp)


@pylsp.test(config=CONFIG, initialize=False)
async def positional(lsp: pylsp.ALSLanguageClient) -> None:
    await check_completions(lsp, threshold=0)


@pylsp.test(config=CONFIG, initialize=False)
async def snippets_disabled(lsp: pylsp.ALSLanguageClient) -> None:
    await check_completions(lsp, use_snippets=False)


@pylsp.test(config=CONFIG, initialize=False)
async def snippets_unsupported(lsp: pylsp.ALSLanguageClient) -> None:
    await check_completions(lsp, snippet_support=False)
