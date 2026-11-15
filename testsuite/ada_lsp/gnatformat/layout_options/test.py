"""Apply the CLI's layout and override behavior through GPR attributes."""

from pathlib import Path

from drivers.lsp_ada_requests import apply_text_edits
from drivers.pylsp import ALSLanguageClient, assertEqual, test
from lsprotocol.types import (
    DocumentFormattingParams,
    DocumentRangeFormattingParams,
    FormattingOptions,
    Position,
    Range,
    TextDocumentIdentifier,
)


# Expected contents were checked against the GNATformat CLI with this project.
EXPECTED = {
    "main": (
        "procedure Main\nis\n"
        "   procedure My_Proc\n"
        "     (A : Integer;\n      B : Boolean;\n      C : Float)\n"
        "   is null;\nbegin\n   null;\nend Main;\n"
    ),
    "default_layout": (
        "procedure Default_Layout is\n"
        "   procedure My_Proc (A : Integer; B : Boolean; C : Float) is null;\n"
        "begin\n   null;\nend Default_Layout;\n"
    ),
    "overridden": (
        "PROCEDURE Overridden\nIS\n"
        "   PROCEDURE My_Proc\n"
        "     (A : Integer;\n      B : Boolean;\n      C : Float)\n"
        "   IS NULL;\nBEGIN\n   NULL;\nEND Overridden;\n"
    ),
}


@test(als_settings={"projectFile": "default.gpr"})
async def layout_and_overrides(lsp: ALSLanguageClient):
    options = FormattingOptions(tab_size=3, insert_spaces=True)
    documents = {
        name: TextDocumentIdentifier(lsp.didOpenFile(name + ".adb"))
        for name in EXPECTED
    }

    async def check():
        for name, document in documents.items():
            for edits in (
                await lsp.text_document_formatting_async(
                    DocumentFormattingParams(document, options)
                ),
                await lsp.text_document_range_formatting_async(
                    DocumentRangeFormattingParams(
                        document, Range(Position(0, 0), Position(5, 0)), options
                    )
                ),
            ):
                assert edits, "expected formatting edits"
                formatted = apply_text_edits(Path(name + ".adb").read_text(), edits)
                # The CLI pipe writer adds a terminal newline of its own.
                assertEqual(formatted.rstrip("\n"), EXPECTED[name].rstrip("\n"))

    await check()
    await check()  # Cached configurations retain the same layout and overrides.

    # File edits alone must not refresh the cache. A project reload or restart
    # is required to load changes to an already cached override.
    override = Path("lower.json")
    original = override.read_text()
    try:
        override.write_text("[]\n")
        await check()
    finally:
        override.write_text(original)
