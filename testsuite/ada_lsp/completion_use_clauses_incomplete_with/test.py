"""
Check that when the with-clause is incomplete (e.g. 'with Ada.'),
use-clause completion is not proposed.
Use-clause completion should only be triggered after a properly
terminated with-clause (i.e. after the ';').
"""

from drivers import pylsp
from lsprotocol.types import (
    CompletionContext,
    CompletionList,
    CompletionParams,
    CompletionTriggerKind,
    TextDocumentIdentifier,
)


@pylsp.test()
async def test(lsp: pylsp.ALSLanguageClient) -> None:
    file_uri = lsp.didOpenFile("main.adb")
    await lsp.awaitIndexingEnd()

    # Trigger completion after an incomplete with-clause (e.g. 'with Ada.')
    result = await lsp.text_document_completion_async(
        CompletionParams(
            TextDocumentIdentifier(file_uri),
            pylsp.Pos(1, 10),
            CompletionContext(
                CompletionTriggerKind.TriggerCharacter, trigger_character="."
            ),
        )
    )

    assert isinstance(result, CompletionList), "Expected a CompletionList result"

    # No use-clause completion item should be proposed for an incomplete with-clause
    use_clause_items = [item for item in result.items if item.label.startswith("use ")]
    lsp.assertEqual(
        use_clause_items,
        [],
    )
