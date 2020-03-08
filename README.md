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
 * Code folding

We also provide [Visual Studio Code](https://code.visualstudio.com/)
[extension as .vsix file](https://dl.bintray.com/reznikmm/ada-language-server/ada-20.0.999.vsix).

## Install

You can install
[binary image](https://bintray.com/reznikmm/ada-language-server/ada-language-server#files)
or build language server from sources.

To install binary image download an archive corresponding to your OS and unpack it
somewhere. You will find `ada_language_server` inside unpacked folder.
We provide binaries for
 * Linux x86_64 - take
[linux.tar.gz](https://dl.bintray.com/reznikmm/ada-language-server/linux-latest.tar.gz)
 * Window 64 bit - take
[win32.zip](https://dl.bintray.com/reznikmm/ada-language-server/win32-latest.zip)
 * Mac OS X - take
[darwin.tar.gz](https://dl.bintray.com/reznikmm/ada-language-server/darwin-latest.tar.gz)

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

# Supported LSP Server Requests

### General Requests

| Request                               | Supported          |
| :------------------------------------ | :----------------: |
| `initialize`                          | :white_check_mark: |
| `initialized`                         | :white_check_mark: |
| `shutdown`                            | :white_check_mark: |
| `exit`                                | :white_check_mark: |
| `$/cancelRequest`                     | :white_check_mark: |

### Workspace Requests

| Request                               | Supported          |
| :------------------------------------ | :----------------: |
| `workspace/didChangeWorkspaceFolders` |                    |
| `workspace/didChangeConfiguration`    | :white_check_mark: |
| `workspace/didChangeWorkspaceFolders` |                    |
| `workspace/didChangeWatchedFiles`     |                    |
| `workspace/symbol`                    |                    |
| `workspace/executeCommand`            | :white_check_mark: |

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
| `textDocument/hover`                  | :white_check_mark: |
| `textDocument/signatureHelp`          |                    |
| `textDocument/definition`             | :white_check_mark: |
| `textDocument/declaration`            | :white_check_mark: |
| `textDocument/typeDefinition`         | :white_check_mark: |
| `textDocument/implementation`         | :white_check_mark: |
| `textDocument/references`             | :white_check_mark: |
| `textDocument/documentHighlight`      |                    |
| `textDocument/documentSymbol`         | :white_check_mark: |
| `textDocument/codeAction`             | :white_check_mark: |
| `textDocument/codeLens`               |                    |
| `codeLens/resolve`                    |                    |
| `textDocument/documentLink`           |                    |
| `documentLink/resolve`                |                    |
| `textDocument/documentColor`          |                    |
| `textDocument/colorPresentation`      |                    |
| `textDocument/formatting`             |                    |
| `textDocument/rangeFormatting`        |                    |
| `textDocument/onTypeFormatting`       |                    |
| `textDocument/rename`                 | :white_check_mark: |
| `textDocument/prepareRename`          |                    |
| `textDocument/foldingRange`           | :white_check_mark: |

### Protocol extensions

The Ada Language Server supports some features that are not in the official
[Language Server Protocol](https://microsoft.github.io/language-server-protocol)
specification. See [corresponding document](doc/README.md).

# How to use the VScode extension

For the moment, this repository includes a vscode extension that is used as the
reference extension for this implementation.

You can try it by running:

```
code --extensionDevelopmentPath=<path_to_this_repo>/integration/vscode/ada <workspace directory>
```

You can configure the [GNAT Project File]() and scenario variables via the
`.vscode/settings.json` settings file, via the keys `"ada.projectFile"` and
`"ada.scenarioVariables"`.

You can set the character set to use when the server has to use when reading
files from disk by specifying an `"ada.defaultCharset"` key. The default is
`iso-8859-1`.

You can explicitly deactivate the emission of diagnostics, via the
`"ada.enableDiagnostics"` key. By default, diagnostics are enabled.

The language server is able to edit Ada comments while executing
`textDocument/rename` request. To enable this just set
`ada.renameInComments` setting to `true`.

By default, the server indexes the source files after loading a project,
to speed up subsequent requests. This behavior can be controlled
via the `"ada.enableIndexing"` flag in this request.

Here is an example config file from the gnatcov project:

```json
{
    "ada.projectFile": "gnatcov.gpr",
    "ada.scenarioVariables": {
        "BINUTILS_BUILD_DIR": "/null",
        "BINUTILS_SRC_DIR": "/null"
    },
   "ada.defaultCharset": "utf-8",
   "ada.enableDiagnostics": false,
   "ada.renameInComments": false
}
```

# Integration with LanguageClient-Neovim

If you want to integrate the Ada Language Server into Neovim, you can use the
[LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim).

You'll have to [install](#install) the Ada Language Server manually somewhere on your
computer, and then you can add the following line to your `init.vim` file:

```viml
" replace the path below with the proper path to the ada_language_server executable
let g:LanguageClient_serverCommands = {
    \ 'ada': ['path/to/ada_language_server'],
    \ }
" if you already have LanguageClient_serverCommands, just add a line for ada.
```

To configure the Ada Language Server for a specific workspace/project, you can
use the `.vim/settings.json` file. It is mandatory as soon as you want to use a
specific `.gpr` project file.

This is the way to specify a project file, eg. you cannot open a project file
another way.

Here is an example of a settings file:

```json
{
    "ada.projectFile": "project.gpr",
    "ada.scenarioVariables": {
        "GLFW_Version": "3",
        "GLFW_Lib": "-lglfw",
        "Windowing_System": "x11"
    }
}
```

The location where the `.vim` folder is located will determine the relative
path of the project file (so no need to prefix with `..`). When vim is opened
in the folder containing this `.vim` directory, it will use those settings for
the language server *even for files which might have nothing to do with that
specific project*, so this needs to be taken into account. Ultimately what this
means is that the configuration is determined by where you open vim.

# Integration with emacs lsp-mode

The configuration for each project can be provided using a `.dir-locals.el`
file defined at the root of each project.

The scenario variables should be declared in your `.emacs` or any loaded
Emacs configuration file.

```elisp
(defgroup project-build nil
  "LSP options for Project"
  :group 'ada-mode)

(defcustom project-build-type "Debug"
  "Controls the type of build of a project.
   Default is Debug, other choices are Release and Coverage."
  :type '(choice
          (const "Debug")
          (const "Coverage")
          (const "Release"))
  :group 'project-build)
```

Your `.dir-locals.el` in the project root should be similar to:

```elisp
((ada-mode .
  ((eval . (lsp-register-custom-settings
      '(("ada.scenarioVariables.BINUTILS_SRC_DIR" project-binutils-dir)
        ("ada.scenarioVariables.BUILD_TYPE" project-build-type "Release"))))
   (lsp-ada-project-file . "/home/username/project/project.gpr"))
  ))
```

The [lsp-mode](https://github.com/emacs-lsp/lsp-mode) provides built-in support
for the `ada_language_server` and defines default customizable configuration
values in the `lsp-ada` group that can be edited similarly to
`lsp-ada-project-file` in the example above.

## Authors & Contributors

 * Maintained by [AdaCore](https://www.adacore.com).
 * Original author [@MaximReznik](https://github.com/reznikmm).
 * Support for the Visual Studio Code classifier and snippets contributed by [@Entomy](https://github.com/Entomy).

## Contribute

Feel free to dive in! Read the [developer's guide](doc/HACKING.md).

Don't hesitate to [open an issue](https://github.com/AdaCore/ada_language_server/issues/new) or submit PRs.

## License

[GPL-3](LICENSE)
