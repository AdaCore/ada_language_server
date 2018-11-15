# Ada Language Server

This repository contains a prototype implementation of the Microsoft Language
Server Protocol for Ada/SPARK.

# Supported LSP Server Requests

### General Requests

| Request                               | Supported          |
| :------------------------------------ | :----------------: |
| `initialize`                          | :white_check_mark: |
| `initialized`                         | :white_check_mark: |
| `shutdown`                            | :white_check_mark: |
| `exit`                                | :white_check_mark: |
| `$/cancelRequest`                     |                    |

### Workspace Requests

| Request                               | Supported          |
| :------------------------------------ | :----------------: |
| `workspace/didChangeWorkspaceFolders` |                    |
| `workspace/didChangeConfiguration`    | :white_check_mark: |
| `workspace/didChangeWatchedFiles`     |                    |
| `workspace/symbol`                    |                    |
| `workspace/executeCommand`            |                    |

### Synchronization Requests

| Request                               | Supported          |
| :------------------------------------ | :----------------: |
| `textDocument/didOpen`                | :white_check_mark: |
| `textDocument/didChange`              | :white_check_mark: |
| `textDocument/willSave`               |                    |
| `textDocument/willSaveWaitUntil`      |                    |
| `textDocument/didSave`                |                    |
| `textDocument/didClose`               | :white_check_mark: |

### Text Document Requests

| Request                               | Supported          |
| :------------------------------------ | :----------------: |
| `textDocument/completion`             | :white_check_mark: |
| `completionItem/resolve`              |                    |
| `textDocument/hover`                  |                    |
| `textDocument/signatureHelp`          |                    |
| `textDocument/definition`             | :white_check_mark: |
| `textDocument/typeDefinition`         |                    |
| `textDocument/implementation`         |                    |
| `textDocument/references`             | :white_check_mark: |
| `textDocument/documentHighlight`      |                    |
| `textDocument/documentSymbol`         | :white_check_mark: |
| `textDocument/codeAction`             |                    |
| `textDocument/codeLens`               |                    |
| `codeLens/resolve`                    |                    |
| `textDocument/documentLink`           |                    |
| `documentLink/resolve`                |                    |
| `textDocument/documentColor`          |                    |
| `textDocument/colorPresentation`      |                    |
| `textDocument/formatting`             |                    |
| `textDocument/rangeFormatting`        |                    |
| `textDocument/onTypeFormatting`       |                    |
| `textDocument/rename`                 |                    |
| `textDocument/prepareRename`          |                    |
| `textDocument/foldingRange`           |                    |

# How to build

This projects needs at least a version of the GNAT compiler, and the
[Libadalang](https://github.com/AdaCore/libadalang) library to be built and
available via the `GPR_PROJECT_PATH`.

Then you can just run `make` in the current directory.

# How to use

For the moment, this repository includes a vscode extension that is used as the
reference extension for this implementation.

You can try it by running:

```
code --extensionDevelopmentPath=<path_to_this_repo>/integration/vscode/ada <workspace directory>
```

You can configure the [GNAT Project File]() and scenario variables via the
`.vscode/settings.json` settings file, via the keys `"ada.projectFile"` and
`"ada.scenarioVariables"`.

Here is an example config file from the gnatcov project:

```json
{
    "ada.projectFile": "gnatcov.gpr",
    "ada.scenarioVariables": {
        "BINUTILS_BUILD_DIR": "/null",
        "BINUTILS_SRC_DIR": "/null"
    }
}
```
