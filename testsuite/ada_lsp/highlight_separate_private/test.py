"""Check highlighting of separate procedures and private type completions."""

from pathlib import Path

from drivers.pylsp import ALSLanguageClient, URI, test
from lsprotocol import types


@test(initialize=False)
async def test_highlighting(lsp: ALSLanguageClient) -> None:
    semantic_tokens = types.SemanticTokensClientCapabilities(
        requests=types.SemanticTokensClientCapabilitiesRequestsType(
            full=True, range=True
        ),
        token_types=["type", "struct", "function"],
        token_modifiers=["declaration", "definition"],
        formats=[types.TokenFormat.Relative],
    )
    result = await lsp.initialize_session(
        types.InitializeParams(
            root_uri=URI("."),
            capabilities=types.ClientCapabilities(
                text_document=types.TextDocumentClientCapabilities(
                    semantic_tokens=semantic_tokens
                )
            ),
        )
    )
    provider = result.capabilities.semantic_tokens_provider
    assert provider is not None
    lsp.assertEqual(provider.legend.token_modifiers, ["declaration", "definition"])

    # In this legend, declaration is 1 and definition is 2.
    # References have neither modifier.
    declaration, definition, reference = 1, 2, 0
    expected = {
        "pkg.ads": {
            # Private view, parameter type reference, then the record completion.
            "Private_Type": [declaration, reference, definition],
            # The spec is the first declaration of the separate procedure.
            "Separate_Proc": [declaration],
        },
        "pkg.adb": {
            # The stub completes the spec; its call is still a reference.
            "Separate_Proc": [definition, reference],
            # Using the private type in a parameter does not declare it again.
            "Private_Type": [reference],
        },
        "pkg-separate_proc.adb": {
            # The separate body and its end name are both definitions.
            "Separate_Proc": [definition, definition],
        },
    }

    lsp.didChangeConfig({"projectFile": "default.gpr"})
    await lsp.awaitIndexingEnd()

    for filename, expected_tokens in expected.items():
        document = types.TextDocumentIdentifier(lsp.didOpenFile(filename))
        lines = Path(filename).read_text().splitlines()
        full = await lsp.text_document_semantic_tokens_full_async(
            types.SemanticTokensParams(document)
        )
        ranged = await lsp.text_document_semantic_tokens_range_async(
            types.SemanticTokensRangeParams(
                document,
                types.Range(types.Position(0, 0), types.Position(len(lines), 0)),
            )
        )
        assert full is not None and ranged is not None
        # Both requests must return the same tokens for the whole file.
        lsp.assertEqual(ranged.data, full.data)

        # Find the named tokens using the LSP line and column deltas, then
        # compare their modifiers in source order.
        actual: dict[str, list[int]] = {name: [] for name in expected_tokens}
        line = column = 0
        for index in range(0, len(full.data), 5):
            delta_line, delta_column, length, _, modifiers = full.data[
                index : index + 5
            ]
            line += delta_line
            column = (column if delta_line == 0 else 0) + delta_column
            name = lines[line][column : column + length]
            if name in actual:
                actual[name].append(modifiers)
        lsp.assertEqual(actual, expected_tokens)
