
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

- [Ada Language Server](#ada-language-server)
  - [Table of Contents](#table-of-contents)
  - [Install](#install)
    - [Dependencies](#dependencies)
  - [Usage](#usage)
  - [Supported LSP Server Requests](#supported-lsp-server-requests)
    - [General Requests](#general-requests)
    - [Workspace Requests](#workspace-requests)
    - [Synchronization Requests](#synchronization-requests)
    - [Text Document Requests](#text-document-requests)
    - [Protocol extensions](#protocol-extensions)
  - [How to use the VScode extension](#how-to-use-the-vscode-extension)
    - [Getting started](#getting-started)
    - [Auto-detected tasks](#auto-detected-tasks)
      - [Short demo](#short-demo)
    - [Go to other Ada file](#go-to-other-ada-file)
    - [Launch the extension to debug it](#launch-the-extension-to-debug-it)
    - [Configuration](#configuration)
  - [Integration with Coc.NVim](#integration-with-cocnvim)
  - [Integration with vim-lsp](#integration-with-vim-lsp)
  - [Integration with LanguageClient-Neovim](#integration-with-languageclient-neovim)
  - [Integration with Neovim's built-in LSP client](#integration-with-neovims-built-in-lsp-client)
    - [Integration with emacs lsp-mode](#integration-with-emacs-lsp-mode)
  - [Integration with QtCreator](#integration-with-qtcreator)
  - [Refactoring Tools](#refactoring-tools)
  - [Authors & Contributors](#authors--contributors)
  - [Contribute](#contribute)
  - [License](#license)

## Install

You can build language server from sources.
To build it from sources install dependencies and run

```sh
make
```

It will build `.obj/server/ada_language_server` file.

### Dependencies

To build the language server you need:

* The GNAT compiler (at least GCC 11 or GNAT Community Edition 2021)
* The [Libadalang](https://github.com/AdaCore/libadalang) library (it should be
  built)
* The [Libadalang-tools](https://github.com/AdaCore/libadalang-tools) library
* The [VSS](https://github.com/AdaCore/VSS) library
* The [gnatdoc](https://github.com/AdaCore/gnatdoc) library
* The [gpr](https://github.com/AdaCore/gpr) library
* The a process [spawn](https://github.com/AdaCore/spawn) library

Project files of the libraries must be available via the `GPR_PROJECT_PATH`
environment variable.

To run the language server you need `gnatls` (parts of GNAT installation)
somewhere in the path.

## Usage

The `ada_language_server` doesn't require any command line options,
but it understands these options:

* `--tracefile=<FILE>` - Full path to a file containing traces configuration
* `--help` - Display supported command like options and exit.

You can turn some debugging and experimental features trought
[the traces file](doc/traces.md).

The server also gets configuration via `workspace/didChangeConfiguration`
notification. See more [details here](doc/settings.md). Each LSP
client provides its-own way to set such settings.

## Supported LSP Server Requests

See [WiKi page](https://github.com/AdaCore/ada_language_server/wiki/Supported-LSP-requests)
for the list of supported requests.

### Protocol extensions

The Ada Language Server supports some features that are not in the official
[Language Server Protocol](https://microsoft.github.io/language-server-protocol)
specification. See [corresponding document](doc/README.md).

## How to use the VScode extension

### Current limitations and differences compared to GNAT Studio

The VSCode extension has a few limitations and some differences compared to [GNAT Studio](https://github.com/AdaCore/gnatstudio):

* **Indentation/formatting**: it does not support automatic indentation when adding a newline and range/document
formatting might no succeed on incomplete/illegal code.

* **Toooling support**: we currently provide minimal support for *SPARK* (see *Prove/Examine* tasks in the [Auto-detected tasks](#auto-detected-tasks) section), but there is no support for tools such as *CodePeer*, *GNATcheck*, *GNATtest* or *GNATcoverage*.

* **Project support**: there is no `Scenario` view: users should configure scenarios via the *ada.scenarioVariables* setting (see the settings list available [here](doc/refactoring_tools.md)). You can execute the *Developper: Reload Window* command to reload your project after saving the new scenario values (use the *Ctrl+P* shortcut to invoke the **Command Palette**, allowing you to execute commands).

  Source directories from imported projects should be added in a [workspace file](https://code.visualstudio.com/docs/editor/workspaces#_multiroot-workspaces). If you already have a workspace file, the extension will propose you to automatically add all the source directories coming from imported projects to your workspace automatically at startup.

* **Debugging**: some manual work is needed in order to debug Ada code. In particular, you will need a `launch.json` file that describes your debugging configuration. All the steps needed for debugging are described [here](https://github.com/AdaCore/ada_language_server/wiki/Getting-Started#debug-hello_worldadb).



### Getting started

[Tutorial: Using Ada in VS Code](https://github.com/AdaCore/ada_language_server/wiki/Getting-Started).

### Refactoring

See a [dedicated document](doc/refactoring_tools.md) with the list of available refactorings.

### Auto-detected tasks

The extension includes a task provider. It provides the following "auto-detected" tasks
(under `/Terminal/Run Task...` menu):

* "ada: Build current project" - launch `gprbuild` to build the current GPR project
* "ada: Check current file" - launch `gprbuild` to check errors in the current editor
* "ada: Clean current project" - launch `gprclean` to clean the current GPR project
* "ada: Examine project" - launch `gnatprove` in flow analysis mode on the current GPR project
* "ada: Examine file" - launch `gnatprove` in flow analysis mode on the file in the current editor
* "ada: Examine subprogram" - launch `gnatprove` in flow analysis mode on the current subprogram in the current editor
* "ada: Prove project" - launch `gnatprove` on the current GPR project
* "ada: Prove file" - launch `gnatprove` on the file in the current editor
* "ada: Prove subprogram" - launch `gnatprove` on the current subprogram in the current editor
* "ada: Prove selected region" - launch `gnatprove` on the selected region in the current editor
* "ada: Prove line" - launch `gnatprove` on the cursor line in the current editor

You can bind keyboard shortcuts to them by adding to the `keybindings.json` file:

```json
{
  "key": "alt+v",
  "command": "workbench.action.tasks.runTask",
  "args": "ada: Check current file",
  "when": "editorLangId == ada"
}
```

#### Short demo

[A demo for auto-detected tasks](https://github.com/AdaCore/ada_language_server/wiki/auto_detected_tasks.mp4)

### Go to other Ada file

The extension contributes a command and a corresponding key binding to
switch between specification and implementation Ada files.
The default shortcut is `Alt+O`.

### Launch the extension to debug it

For the moment, this repository includes a vscode extension that is used as the
reference extension for this implementation.

You can try it by running:

```sh
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

## Integration with Coc.NVim

If you want to use the Ada Language Server with Vim/Neovim, you can use the
[Coc.NVim](https://github.com/neoclide/coc.nvim). You'll have to
[install](#install) the Ada Language Server manually somewhere on your
computer. Follow installation instructions on Coc.NVim website and then
configure the Ada Language Server with `:CocConfig`:

```json
{
  "languageserver": {
    "ada": {
      "settings": {
        "ada": {
          "projectFile": "gnat/vss_text.gpr"
        }
      },
      "command": "<path>/ada_language_server",
      "filetypes": [
        "ads",
        "adb",
        "ada"
      ]
    }
  }
}
```

## Integration with vim-lsp

If you want to integrate the Ada Language Server into vim, you can use the
[vim-lsp](https://github.com/prabirshrestha/vim-lsp).

You'll have to [install](#install) the Ada Language Server manually somewhere on your
computer, and then you can add the following line to your `.vimrc` file:

```viml
if executable('ada_language_server')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'ada_language_server',
        \ 'cmd': ['ada_language_server'],
        \ 'allowlist': ['ada'],
        \ 'workspace_config': {'ada': {
        \     'projectFile': "project.gpr",
        \     'scenarioVariables': {"ARCH": "x86_64-pc-linux-gnu"}}},
        \ })
endif
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

* Install the ada language server and make sure it's in your $PATH.
* Use your favorite Neovim plugin manager to add the default set of [LSP
  configuration files](https://github.com/neovim/nvim-lspconfig) to Neovim.
* Enable the Ada Language Server by adding `:lua require('lspconfig').als.setup{}` to
  your init.vim.

If you would rather not have the ada language server in your path, you can give
the lsp client an absolute path to the ALS executable:

```lua
require('lspconfig').als.setup{ cmd = "/path/to/als/executable" }
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

The Ada Language Server's settings are described [here](doc/settings.md).
Configuring neovim to use project-specific settings is described neovim's
[lspconfig wiki](https://github.com/neovim/nvim-lspconfig/wiki/Project-local-settings)

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

## Refactoring Tools

See [corresponding document](doc/refactoring_tools.md).

## Authors & Contributors

* Maintained by [AdaCore](https://www.adacore.com).
* Original author [@MaximReznik](https://github.com/reznikmm).
* Support for the Visual Studio Code classifier and snippets contributed by [@Entomy](https://github.com/Entomy).

## Contribute

Feel free to dive in! Read the [developer's guide](doc/HACKING.md).

Don't hesitate to [open an issue](https://github.com/AdaCore/ada_language_server/issues/new) or submit PRs.

## License

[GPL-3](LICENSE)
