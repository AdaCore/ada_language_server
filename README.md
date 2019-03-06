# Ada Language Server

[![Build Status](https://travis-ci.org/AdaCore/ada_language_server.svg?branch=master)](https://travis-ci.org/AdaCore/ada_language_server)
[ ![Download](https://api.bintray.com/packages/reznikmm/ada-language-server/ada-language-server/images/download.svg) ](https://bintray.com/reznikmm/ada-language-server/ada-language-server/_latestVersion)

This repository contains a prototype implementation of the [Microsoft Language Server Protocol](https://microsoft.github.io/language-server-protocol/) 
for Ada/SPARK.

Current features:
 * [GNAT project files](https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/gnat_project_manager.html)
 * Code completion
 * Go to definition
 * Find corresponding references
 * Document symbol search

We also provide [Visual Studio Code](https://code.visualstudio.com/)
[extension as .vsix file](https://dl.bintray.com/reznikmm/ada-language-server/ada-0.0.1.vsix).

## Install

You can install
[binary image](https://bintray.com/reznikmm/ada-language-server/ada-language-server#files)
or build language server from sources.

To install binary image download an archive corresponding to your OS and unpack it
somewhere. You will find `ada_language_server` inside unpacked folder.
We provide binaries for
 * Linux x86_64 - take
[linux.tar.gz](https://dl.bintray.com/reznikmm/ada-language-server/linux.tar.gz)
 * Window 64 bit - take
[win32.tar.gz](https://dl.bintray.com/reznikmm/ada-language-server/win32.tar.gz)
 * Mac OS X - take
[darwin.tar.gz](https://dl.bintray.com/reznikmm/ada-language-server/darwin.tar.gz)

To build is from source install dependencies and run
```
make
```

It will build `.obj/server/ada_language_server` file.

### Dependencies

To build the language server you need at least a version of the GNAT compiler,
and the [Libadalang](https://github.com/AdaCore/libadalang) library to be built
and available via the `GPR_PROJECT_PATH`.

To run the language server you need `gnatls` (parts of GNAT installation)
somewhere in the path.

## Usage

The `ada_language_server` doesn't require/understand any command line options.

## Debugging

You can activate traces that show all the server input/output. This is done
by creating a file `$HOME/.als/traces.cfg` with the following contents:

```
ALS.IN=yes > in.txt:buffer_size=0
ALS.OUT=yes > out.txt:buffer_size=0
```

When this is present, the ALS will generate files `$HOME/.als/in.txt` and
`$HOME/.als/out.txt` which contain, respectively, the JSON received and sent
by the ALS.

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

# How to use VScode extension

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

## Maintainer

[@MaximReznik](https://github.com/reznikmm).

## Contribute

Feel free to dive in!
[Open an issue](https://github.com/AdaCore/ada_language_server/issues/new) or submit PRs.

## License

[GPL-3](LICENSE)
