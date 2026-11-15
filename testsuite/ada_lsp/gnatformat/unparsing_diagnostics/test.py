"""Report layout loading failures instead of formatting with a default layout."""

import os
from pathlib import Path

from drivers.lsp_ada_requests import apply_text_edits
from drivers.pylsp import (
    ALSClientServerConfig,
    ALSLanguageClient,
    ALSSettings,
    URI,
    assertEqual,
    test,
)
from lsprotocol.types import (
    ClientCapabilities,
    CompletionClientCapabilities,
    CompletionClientCapabilitiesCompletionItemType,
    CompletionList,
    CompletionParams,
    DocumentFormattingParams,
    DocumentRangeFormattingParams,
    FormattingOptions,
    InitializeParams,
    InsertTextFormat,
    InsertTextMode,
    Position,
    Range,
    TextDocumentClientCapabilities,
    TextDocumentIdentifier,
)
from pygls.exceptions import JsonRpcException


SEMANTIC_LAYOUT = (
    "semantic_layout_with_a_long_name_to_check_that_the_complete_configuration_"
    "diagnostic_is_returned_to_the_client_without_truncation.json"
)


class RangeFormattingSettings(ALSSettings):
    rangeFormatting: dict[str, str]


@test(als_settings={"projectFile": "default.gpr"})
async def formatting_errors(lsp: ALSLanguageClient):
    await check_formatting_errors(lsp)


@test(
    als_settings=RangeFormattingSettings(
        projectFile="default.gpr",
        rangeFormatting={"formatChoice": "narrow"},
    )
)
async def narrow_formatting_errors(lsp: ALSLanguageClient):
    await check_formatting_errors(lsp)


async def check_formatting_errors(lsp: ALSLanguageClient):
    options = FormattingOptions(tab_size=3, insert_spaces=True)
    span = Range(Position(1, 0), Position(3, 0))

    for name, override, diagnostic in (
        ("syntax", "syntax.json", "3:1:"),
        ("syntax_again", "syntax.json", "3:1:"),
        ("semantic", SEMANTIC_LAYOUT, "invalid node name: NoSuchNode"),
        ("missing", "missing.json", "missing.json"),
        (
            "non_object",
            "non_object.json",
            "unparsing configuration must be a JSON object",
        ),
    ):
        filename = name + ".adb"
        document = TextDocumentIdentifier(lsp.didOpenFile(filename))
        previous = None
        # Both formatting modes must report the original diagnostics on cache
        # hits, rather than returning edits or a generic parsing error.
        for _ in range(2):
            for request in (
                lambda document=document: lsp.text_document_formatting_async(
                    DocumentFormattingParams(document, options)
                ),
                lambda document=document: lsp.text_document_range_formatting_async(
                    DocumentRangeFormattingParams(document, span, options)
                ),
            ):
                try:
                    await request()
                except JsonRpcException as error:
                    assertEqual(error.code, -32803)  # RequestFailed
                    assert error.message.startswith(
                        "GNATformat: could not load unparsing configuration for "
                        + filename
                    ), error.message
                    assert (
                        "Layout override: " + str(Path(override).resolve())
                        in error.message
                    ), error.message
                    assert diagnostic in error.message, error.message
                    if name == "semantic":
                        assert len(error.message) > 200
                    if previous is not None:
                        assertEqual(error.message, previous)
                    previous = error.message
                else:
                    raise AssertionError("Expected a layout loading error")

    # A failed source-specific configuration must not poison other sources.
    document = TextDocumentIdentifier(lsp.didOpenFile("valid.adb"))
    expected = "procedure Valid is\nbegin\n   null;\nend Valid;\n"
    for edits in (
        await lsp.text_document_formatting_async(
            DocumentFormattingParams(document, options)
        ),
        await lsp.text_document_range_formatting_async(
            DocumentRangeFormattingParams(document, span, options)
        ),
    ):
        assert edits, "expected edits for a valid configuration"
        assertEqual(apply_text_edits(Path("valid.adb").read_text(), edits), expected)


@test(
    config=ALSClientServerConfig(
        [os.environ.get("ALS", "ada_language_server"), "--tracefile=traces.cfg"]
    ),
    initialize=False,
)
async def preserve_completion(lsp: ALSLanguageClient):
    await lsp.initialize_session(
        InitializeParams(
            capabilities=ClientCapabilities(
                text_document=TextDocumentClientCapabilities(
                    completion=CompletionClientCapabilities(
                        completion_item=CompletionClientCapabilitiesCompletionItemType(
                            snippet_support=True
                        )
                    )
                )
            ),
            root_uri=URI("."),
            initialization_options={
                "ada": {"projectFile": "default.gpr", "useCompletionSnippets": True}
            },
        )
    )
    uri = lsp.didOpenFile("snippet.adb")
    previous = None
    for _ in range(2):
        result = await lsp.text_document_completion_async(
            CompletionParams(TextDocumentIdentifier(uri), Position(3, 9))
        )
        assert isinstance(result, CompletionList)
        item = next(item for item in result.items if item.label == "Call_Me")
        assert item.insert_text_format == InsertTextFormat.Snippet
        assert item.text_edit is None
        text = item.insert_text
        assert text is not None
        assert text.startswith("Call_Me"), text
        assert text.endswith(")$0"), text
        assert text.count("${") == 2, text
        assert "foobar_" not in text.lower(), text
        # AsIs is set only after formatting succeeds. A loading failure must
        # leave the generated snippet untouched instead of using a default.
        assert item.insert_text_mode != InsertTextMode.AsIs
        if previous is not None:
            assertEqual(text, previous)
        previous = text
