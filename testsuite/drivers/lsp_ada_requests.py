"""Functions that provide Python dictionary versions of
   LSP requests for testing purposes for the Ada language server in Ada mode.
"""

import os
from attr import dataclass
from drivers.lsp_types import LSPMessage, URI
from typing import List
from lsprotocol.types import (
    DidChangeTextDocumentParams,
    DocumentOnTypeFormattingParams,
    Position,
    Range,
    TextEdit,
    TextDocumentIdentifier,
    TextDocumentContentChangeEvent_Type1,
    VersionedTextDocumentIdentifier,
)


# TODO: use a library such as pytest-lsp to support most requests


def initialize(workspacefolder=URI(".")):  # noqa: B008
    """This mimics what vs code sends at the moment"""
    return LSPMessage(
        {
            "method": "initialize",
            "params": {
                "capabilities": {
                    "textDocument": {
                        "codeAction": {
                            "codeActionLiteralSupport": {
                                "codeActionKind": {
                                    "valueSet": [
                                        "",
                                        "quickfix",
                                        "refactor",
                                        "refactor.extract",
                                        "refactor.inline",
                                        "refactor.rewrite",
                                        "source",
                                        "source.organizeImports",
                                    ]
                                }
                            },
                            "dynamicRegistration": True,
                        },
                        "codeLens": {"dynamicRegistration": True},
                        "colorProvider": {"dynamicRegistration": True},
                        "completion": {
                            "completionItem": {
                                "commitCharactersSupport": True,
                                "deprecatedSupport": True,
                                "documentationFormat": ["markdown", "plaintext"],
                                "preselectSupport": True,
                                "snippetSupport": True,
                            },
                            "completionItemKind": {"valueSet": list(range(1, 26))},
                            "contextSupport": True,
                            "dynamicRegistration": True,
                        },
                        "declaration": {
                            "dynamicRegistration": True,
                            "linkSupport": True,
                        },
                        "definition": {
                            "dynamicRegistration": True,
                            "linkSupport": True,
                        },
                        "documentHighlight": {"dynamicRegistration": True},
                        "documentLink": {"dynamicRegistration": True},
                        "documentSymbol": {
                            "dynamicRegistration": True,
                            "hierarchicalDocumentSymbolSupport": True,
                            "symbolKind": {"valueSet": list(range(1, 27))},
                        },
                        "foldingRange": {
                            "dynamicRegistration": True,
                            "lineFoldingOnly": True,
                            "rangeLimit": 5000,
                        },
                        "formatting": {"dynamicRegistration": True},
                        "hover": {
                            "contentFormat": ["markdown", "plaintext"],
                            "dynamicRegistration": True,
                        },
                        "implementation": {
                            "dynamicRegistration": True,
                            "linkSupport": True,
                        },
                        "onTypeFormatting": {"dynamicRegistration": True},
                        "publishDiagnostics": {"relatedInformation": True},
                        "rangeFormatting": {"dynamicRegistration": True},
                        "references": {"dynamicRegistration": True},
                        "rename": {"dynamicRegistration": True, "prepareSupport": True},
                        "signatureHelp": {
                            "dynamicRegistration": True,
                            "signatureInformation": {
                                "documentationFormat": ["markdown", "plaintext"],
                                "parameterInformation": {"labelOffsetSupport": True},
                            },
                        },
                        "synchronization": {
                            "didSave": True,
                            "dynamicRegistration": True,
                            "willSave": True,
                            "willSaveWaitUntil": True,
                        },
                        "typeDefinition": {
                            "dynamicRegistration": True,
                            "linkSupport": True,
                        },
                    },
                    "workspace": {
                        "applyEdit": True,
                        "configuration": True,
                        "didChangeConfiguration": {"dynamicRegistration": True},
                        "didChangeWatchedFiles": {"dynamicRegistration": True},
                        "executeCommand": {"dynamicRegistration": True},
                        "symbol": {
                            "dynamicRegistration": True,
                            "symbolKind": {"valueSet": list(range(1, 27))},
                        },
                        "workspaceEdit": {
                            "documentChanges": True,
                            "failureHandling": "textOnlyTransactional",
                            "resourceOperations": ["create", "rename", "delete"],
                        },
                        "workspaceFolders": True,
                    },
                },
                "processId": os.getpid(),
                "trace": "off",
                "workspaceFolders": [
                    {"name": "ada_language_server", "uri": workspacefolder}
                ],
            },
        }
    )


def initialized():
    """Emit an initialized notification"""
    return LSPMessage({"method": "initialized"}, False)


def didChangeConfiguration(
    scenarioVariables={},  # noqa: B006
    defaultCharset="ISO-8859-1",
    adaFileDiagnostics=False,
    followSymlinks=False,
):
    """Emit a didChangeConfiguration notification with our Ada settings"""
    return LSPMessage(
        {
            "method": "workspace/didChangeConfiguration",
            "params": {
                "settings": {
                    "ada": {
                        "scenarioVariables": scenarioVariables,
                        "defaultCharset": defaultCharset,
                        "adaFileDiagnostics": adaFileDiagnostics,
                        "followSymlinks": followSymlinks,
                    }
                }
            },
        },
        False,
    )


def didOpen_from_disk(filename: str, language: str = "ada"):
    """Emit a textdDocument/didOpen notification for the given file,
    with the file contents read from disk, as a convenience.
    """
    with open(filename, "r") as f:
        content = f.read()
    return LSPMessage(
        {
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": URI(filename),
                    "languageId": language,
                    "version": 0,
                    "text": content,
                }
            },
        },
        False,
    )


def prepareCallHierarchy(filename: str, line: int, character: int):
    """Craft a textDocument/prepareCallHierarchy request.
    line and character are specified in 1-based coordinates.
    """
    return LSPMessage(
        {
            "method": "textDocument/prepareCallHierarchy",
            "params": {
                "textDocument": {"uri": URI(filename)},
                "position": {"line": line - 1, "character": character - 1},
            },
        }
    )


def incomingCalls(filename: str, line: int, character: int):
    """Craft a callHierarchy/incomingCalls request.
    line and character are specified in 1-based coordinates.
    """
    return LSPMessage(
        {
            "method": "callHierarchy/incomingCalls",
            "params": {
                "item": {
                    "name": "",
                    "kind": 12,
                    "uri": URI(filename),
                    "range": {
                        "start": {"line": line - 1, "character": character - 1},
                        "end": {"line": line - 1, "character": character - 1},
                    },
                    "selectionRange": {
                        "start": {"line": line - 1, "character": character - 1},
                        "end": {"line": line - 1, "character": character - 1},
                    },
                }
            },
        }
    )


@dataclass
class IndentationTestCase:
    """
    IndentationTestCase is a class that represents a test case for indentation
    handling in onTypeFormatting requests. It contains the following
    attributes:

    Attributes:
        description (str): A brief description of the test case.
        source_filename (str): The filename of the source code to be tested.
            line_break_position (Position): The position in the source code
            where the line break occurs.
        expected_indentation (str): The expected indentation string after the
            onTypeFormatting request is applied.
        additional_text_edits (List[TextEdit]): A list of additional TextEdit
            objects that represent other expected changes in the source code
            (e.g: a TextEdit on the previous line when indentOnly is False).
    """

    description: str
    source_filename: str
    line_break_position: Position
    expected_indentation: str
    additional_text_edits: List[TextEdit] = []

    @property
    def change_range(self) -> Range:
        """
        Returns a Range object representing the range of the line break
        position. The range is defined by the start and end positions, both set
        to the line break position of the current instance.
        """
        return Range(self.line_break_position, self.line_break_position)

    @property
    def on_type_formatting_request_pos(self) -> Position:
        """
        Determines the position for onTypeFormatting request.

        This method calculates the position where the onTypeFormatting should
        be requested. It returns a new position that is one line below the
        current line break position and at the start of the line (column 0).
        """
        return Position(self.line_break_position.line + 1, 0)

    @property
    def expected_text_edits(self) -> List[TextEdit]:
        """
        Generates a expected reponse (list of TextEdit objects) for the
        onTypeFormatting request.
        """
        return [
            TextEdit(
                Range(
                    Position(self.line_break_position.line + 1, 0),
                    Position(self.line_break_position.line + 1, 0),
                ),
                self.expected_indentation,
            )
        ] + self.additional_text_edits


async def run_indentation_testcases(lsp, testcases, options):
    """
    Runs a list of IndentationTestCase and returns a list of failed tests
    """
    failed_tests = []

    # For each test:
    # - Send a didChange notification with a line break
    # - Send a onTypeFormattting request on the new line
    # - Check if the reponse is the expected one

    for indentation_test in testcases:
        try:
            source = VersionedTextDocumentIdentifier(
                uri=lsp.didOpenFile(
                    os.path.join(os.getcwd(), indentation_test.source_filename)
                ),
                version=0,
            )

            lsp.text_document_did_change(
                DidChangeTextDocumentParams(
                    text_document=source,
                    content_changes=[
                        TextDocumentContentChangeEvent_Type1(
                            range=indentation_test.change_range,
                            range_length=0,
                            text="\n",
                        )
                    ],
                )
            )

            text_edits = await lsp.text_document_on_type_formatting_async(
                DocumentOnTypeFormattingParams(
                    text_document=TextDocumentIdentifier(source.uri),
                    position=indentation_test.on_type_formatting_request_pos,
                    ch="\n",
                    options=options,
                )
            )
            lsp.assertEqual(text_edits, indentation_test.expected_text_edits)

        except Exception as e:  # pylint: disable=broad-exception-caught
            failed_tests.append(
                f"Test case '{indentation_test.description}' failed:\n{str(e)}"
            )
    return failed_tests
