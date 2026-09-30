"""
Highlight references of an entity declared in another file.

The test verifies that the references are highlighted correctly when the
declaration is in a different file than the one being edited.
"""

from drivers.pylsp import ALSLanguageClient, Pos, test
from lsprotocol.types import (
    DocumentHighlight,
    DocumentHighlightKind,
    DocumentHighlightParams,
    Range,
    TextDocumentIdentifier,
)


@test()
async def test_highlight_declaration_in_other_file(lsp: ALSLanguageClient) -> None:
    main_adb = lsp.didOpenFile("main.adb")

    result = await lsp.text_document_document_highlight_async(
        DocumentHighlightParams(TextDocumentIdentifier(main_adb), Pos(4, 9))
    )

    lsp.assertEqual(
        result,
        [DocumentHighlight(Range(Pos(4, 8), Pos(4, 11)), DocumentHighlightKind.Read)],
    )
