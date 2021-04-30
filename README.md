
# Ada Language Server

[![Build binaries](https://github.com/AdaCore/ada_language_server/workflows/Build%20binaries/badge.svg)](https://github.com/AdaCore/ada_language_server/actions)
[![GitHub tag (latest by date)](https://img.shields.io/github/v/tag/AdaCore/ada_language_server)](https://github.com/AdaCore/ada_language_server/releases)
[![VS Marketplace](https://img.shields.io/visual-studio-marketplace/v/adacore.ada?label=VS%20Marketplace)](https://marketplace.visualstudio.com/items?itemName=AdaCore.ada)
[![Open VSX Registry](https://img.shields.io/open-vsx/v/AdaCore/ada?label=Open%20VSX)](https://open-vsx.org/extension/AdaCore/ada)
[![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-blue?logo=gitpod)](https://gitpod.io/#https://github.com/AdaCore/ada_language_server/tree/edge)

This repository contains an implementation of the [Microsoft Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
for Ada/SPARK.

Current features:
 * [GNAT project files](https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/gnat_project_manager.html) support.
 * Code completion for names, keywords, aggregates, etc.
 * Code navigation, such as Go to Definition/Declaration, Find All References, Call Hierarchies, etc.
 * Code refactoring like insert named associations, auto-add `with`-clauses.
 * Document/Workspace symbol search.
 * Code folding and formatting.

We also provide [Visual Studio Code](https://code.visualstudio.com/)
extension at
[the VS Marketplace](https://marketplace.visualstudio.com/items?itemName=AdaCore.ada) and at
[the Open VSX Registry](https://open-vsx.org/extension/AdaCore/ada).

## Table of Contents
 * [Install](#Install)
   * [Dependencies](#Dependencies)
 * [Usage](#Usage)
 * [Supported LSP Server Requests](#supported-lsp-server-requests)
   * [General Requests](#General-Requests)
   * [Workspace Requests](#Workspace-Requests)
   * [Synchronization Requests](#Synchronization-Requests)
   * [Text Document Requests](#Text-Document-Requests)
   * [Protocol extensions](#Protocol-extensions)
 * [How to use the VScode extension](#How-to-use-the-VScode-extension)
 * [Integration with LanguageClient-Neovim](#Integration-with-LanguageClient-Neovim)
 * [Integration with Neovim's built-in LSP client](#Integration-with-Neovim's-built-in-LSP-client)
 * [Integration with emacs lsp-mode](#Integration-with-emacs-lsp-mode)
 * [Integration with QtCreator](#Integration-with-QtCreator)
 * [Authors & Contributors](#Authors--Contributors)
 * [Contribute](#Contribute)
 * [License](#License)

## Install

You can build language server from sources.
To build is from sources install dependencies and run
```
make
```

It will build `.obj/server/ada_language_server` file.

### Dependencies

To build the language server you need:

 * A GNAT compiler
 * The [Libadalang](https://github.com/AdaCore/libadalang) library (it should be
   built)
 * The [Libadalang-tools](https://github.com/AdaCore/libadalang-tools) library
 * The [VSS](https://github.com/AdaCore/VSS) library
 * The a process [spawn](https://github.com/AdaCore/spawn) library

Project files of the libraries must be available via the `GPR_PROJECT_PATH`
environment variable.

To run the language server you need `gnatls` (parts of GNAT installation)
somewhere in the path.

## Usage

The `ada_language_server` doesn't require any command line options,
but it understands these options:

 * `--tracefile=<FILE>` - Full path to a file containing traces
   configuration
 * `--help` - Display supported command like options and exit.

You can turn some debugging and experimental features trought
[the traces file](doc/traces.md).

The server also gets configuration via `workspace/didChangeConfiguration`
notification. See more [details here](doc/settings.md). Each LSP
client provides its-own way to set such settings.

## Supported LSP Server Requests

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
| `workspace/didChangeWatchedFiles`     | :white_check_mark: |
| `workspace/symbol`                    | :white_check_mark: |
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
| `textDocument/signatureHelp`          | :white_check_mark: |
| `textDocument/definition`             | :white_check_mark: |
| `textDocument/declaration`            | :white_check_mark: |
| `textDocument/typeDefinition`         | :white_check_mark: |
| `textDocument/implementation`         | :white_check_mark: |
| `textDocument/references`             | :white_check_mark: |
| `textDocument/documentHighlight`      | :white_check_mark: |
| `textDocument/documentSymbol`         | :white_check_mark: |
| `textDocument/codeAction`             | :white_check_mark: |
| `textDocument/codeLens`               |                    |
| `codeLens/resolve`                    |                    |
| `textDocument/documentLink`           |                    |
| `documentLink/resolve`                |                    |
| `textDocument/documentColor`          |                    |
| `textDocument/colorPresentation`      |                    |
| `textDocument/formatting`             | :white_check_mark: |
| `textDocument/rangeFormatting`        |                    |
| `textDocument/onTypeFormatting`       |                    |
| `textDocument/rename`                 | :white_check_mark: |
| `textDocument/prepareRename`          | :white_check_mark: |
| `textDocument/foldingRange`           | :white_check_mark: |
| `textDocument/prepareCallHierarchy`   | :white_check_mark: |
| `callHierarchy/incomingCalls`         | :white_check_mark: |
| `callHierarchy/outgoingCalls`         | :white_check_mark: |

### Protocol extensions

The Ada Language Server supports some features that are not in the official
[Language Server Protocol](https://microsoft.github.io/language-server-protocol)
specification. See [corresponding document](doc/README.md).

## How to use the VScode extension

### Auto-detected tasks

The extension includes a task provider. It provides two "auto-detected" tasks:
* "ada: Build current project" - launch `gprbuild` to build the current GPR project
* "ada: Check current file" - launch `gprbuild` to check errors in the current editor

You can bind keyboard shortcuts to them by adding to the `keybindings.json` file:

```json
{
  "key": "alt+v",
  "command": "workbench.action.tasks.runTask",
  "args": "ada: Check current file",
  "when": "editorLangId == ada"
}
```

### Go to other Ada file

The extension contributes a command and a corresponding key binding to
switch between specification and implementation Ada files.
The default shortcut is `Alt+O`.

### Launch the extension to debug it

For the moment, this repository includes a vscode extension that is used as the
reference extension for this implementation.

You can try it by running:

```
code --extensionDevelopmentPath=<path_to_this_repo>/integration/vscode/ada <workspace directory>
```

### Configuration

You can configure the extension via the `.vscode/settings.json` settings file.
See the setting list [here](doc/settings.md).

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

## Integration with LanguageClient-Neovim

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
See the setting list [here](doc/settings.md).

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

## Integration with Neovim's built-in LSP client

Neovim 0.5.0 and later have a built-in LSP client which can be used with the
Ada Language Server. In order to use it with minimal effort, follow these steps:

- Use your favorite Neovim plugin manager to add the default set of [LSP
  configuration files](https://github.com/neovim/nvim-lsp) to Neovim.
- (Optional) Run `:LspInstall als` to ask Neovim to install the Ada Language
  Server for you.
- Enable the Ada Language Server by adding `:lua require('nvim_lsp').als.setup{}` to
  your init.vim.

If you decided to install the Ada Language Server yourself instead of using
`:LspInstall als`, you will need to specify a command:

```lua
require('nvim_lsp').als.setup{ cmd = "/path/to/als/executable" }
```

Configuring the language server's settings can be achieved like this:

```lua
require('nvim_lsp').als.setup{
  settings = {
    ada = {
      projectFile = "project.gpr";
      scenarioVariables = { ... };
    }
  }
}
```

See the setting list [here](doc/settings.md).

### Integration with emacs lsp-mode

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

## Integration with QtCreator
Starting with version `4.9`, QtCreator supports a LSP plugin. Follow
[the official documentation](https://doc.qt.io/qtcreator/creator-language-servers.html)
to configure the Ada Language Server in this plugin. Make sure to set `Startup behavior`
to `Start Server per Project`, otherwise QtCreator won't provide the project root to
the Ada Language Server. QtCreator doesn't send any configuration request to the language server, so the only
option to enable project support is to have a single `.gpr` file in the QtCreator
project folder. For a projectless configuration, you could also place all Ada sources in
the project root folder, this should work as well.

## Authors & Contributors

 * Maintained by [AdaCore](https://www.adacore.com).
 * Original author [@MaximReznik](https://github.com/reznikmm).
 * Support for the Visual Studio Code classifier and snippets contributed by [@Entomy](https://github.com/Entomy).

## Contribute

Feel free to dive in! Read the [developer's guide](doc/HACKING.md).

Don't hesitate to [open an issue](https://github.com/AdaCore/ada_language_server/issues/new) or submit PRs.

## License

[GPL-3](LICENSE)
