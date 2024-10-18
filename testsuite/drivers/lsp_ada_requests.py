"""Functions that provide Python dictionary versions of
   LSP requests for testing purposes for the Ada language server in Ada mode.
"""

import os
from drivers.lsp_types import LSPMessage, URI

# TODO: use a library such as pytest-lsp to support most requests


def initialize(workspacefolder=URI(".")):
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
    scenarioVariables={},
    defaultCharset="ISO-8859-1",
    enableDiagnostics=False,
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
                        "enableDiagnostics": enableDiagnostics,
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
