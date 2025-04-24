from typing import List
from drivers import pylsp
from lsprotocol.types import (
    Command,
    CompletionContext,
    CompletionItem,
    CompletionList,
    CompletionParams,
    CompletionTriggerKind,
    TextDocumentIdentifier,
)


@pylsp.test()
async def test(lsp: pylsp.ALSLanguageClient) -> None:
    file_uri = lsp.didOpenFile("p1-p3.ads")

    await lsp.awaitIndexingEnd()

    # Complete 'P1.' in 'P1.P3' package spec. This should list
    # the top-level subprogram unit 'P1.P2.Subp' as an invisible
    # completion item.

    result = await lsp.text_document_completion_async(
        CompletionParams(
            TextDocumentIdentifier(file_uri),
            pylsp.Pos(2, 24),
            CompletionContext(CompletionTriggerKind.Invoked),
        )
    )
    EXPECTED_LABEL = "Subp (invisible)"
    EXPECTED_COMMAND = Command(
        title="",
        command="als-auto-import",
        arguments=[
            {
                "context": pylsp.URI("default.gpr"),
                "where": {
                    "textDocument": {"uri": file_uri},
                    "position": {"line": 1, "character": 22},
                },
                "import": "P1.P2.Subp",
                "qualifier": "",
            }
        ],
    )

    def find_expected_completion_item(
        items: List[CompletionItem],
        expected_label,
        expected_command,
    ):
        """
        Find the expected completion item in the given completion items' list.
        """
        expected_items = [
            item
            for item in items
            if item.label == expected_label and item.command == expected_command
        ]
        return len(expected_items) == 1

    # Verify that we are able to find the expected completion invisible item
    # for 'P1.P2.Subp'
    assert isinstance(
        result, CompletionList
    ), "The returned completion list result should be of type CompletionList"
    lsp.assertEqual(
        find_expected_completion_item(result.items, EXPECTED_LABEL, EXPECTED_COMMAND),
        True,
    )
