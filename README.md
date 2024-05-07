
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
* Basic [Alire](https://alire.ada.dev/) support.
* Code completion for names, keywords, aggregates, etc.
* Code navigation, such as Go to Definition/Declaration, Find All References, Call Hierarchies, etc.
* [Code refactoring](#refactoring-tools) like insert named associations, auto-add `with`-clauses, etc.
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
    - [Protocol Extensions](#protocol-extensions)
  - [VS Code Extension](#vs-code-extension)
    - [Getting Started](#getting-started)
    - [Configuration](#configuration)
    - [Refactoring](#refactoring)
    - [Tasks](#tasks)
      - [Task Customization](#task-customization)
    - [Commands and Shortcuts](#commands-and-shortcuts)
      - [Ada: Go to other file](#ada-go-to-other-file)
      - [Ada: Add subprogram box](#ada-add-subprogram-box)
      - [Ada: Reload project](#ada-reload-project)
    - [Bug Reporting](#bug-reporting)
    - [Limitations and Differences with GNAT Studio](#limitations-and-differences-with-gnat-studio)
  - [Integration with other editors and IDEs](#integration-with-other-editors-and-ides)
    - [Integration with Coc.NVim](#integration-with-cocnvim)
    - [Integration with vim-lsp](#integration-with-vim-lsp)
    - [Integration with LanguageClient-Neovim](#integration-with-languageclient-neovim)
    - [Integration with Neovim's built-in LSP client](#integration-with-neovims-built-in-lsp-client)
    - [Integration with emacs lsp-mode](#integration-with-emacs-lsp-mode)
    - [Integration with QtCreator](#integration-with-qtcreator)
  - [Refactoring Tools](#refactoring-tools)
  - [Authors \& Contributors](#authors--contributors)
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
* The process [spawn](https://github.com/AdaCore/spawn) library
* The [lal-refactor](https://github.com/AdaCore/lal-refactor) library
* The [templates-parser](https://github.com/AdaCore/templates-parser) library

Project files of the libraries must be available via the `GPR_PROJECT_PATH`
environment variable.

If you intend to use VS Code on this workspace, it is recommended to check out
these dependencies under `subprojects/` or install them under
`subprojects/prefix`. That will make them automatically visible to the VS Code
Ada extension in this workspace.

To run the language server you need `gnatls` (part of GNAT installation)
somewhere in the path.

## Usage

The `ada_language_server` doesn't require any command line options,
but it understands these options:

* `--tracefile=<FILE>` - Full path to a file containing traces configuration
* `--config=<FILE>` - Full path to a JSON file containing the server's configuration
* `--help` - Display supported command like options and exit.

You can turn some debugging and experimental features through
[the traces file](doc/traces.md).

The server also gets configuration via `workspace/didChangeConfiguration`
notification and `initializationOptions` of `initialize` request.
See more [details here](doc/settings.md). Each LSP
client provides its-own way to set such settings. You can use the `--config`
option if you want to provide the configuration directly via a JSON file
instead of specifying it via the requests listed just above.

## Supported LSP Server Requests

See [WiKi page](https://github.com/AdaCore/ada_language_server/wiki/Supported-LSP-requests)
for the list of supported requests.

### Protocol Extensions

The Ada Language Server supports some features that are not in the official
[Language Server Protocol](https://microsoft.github.io/language-server-protocol)
specification. See [corresponding document](doc/README.md).

## VS Code Extension

A VS Code extension based on this Ada Language Server is available on the [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=AdaCore.ada).
It provides a full set of features including syntax highlighting, navigation, building and debugging.

### Getting Started

Here are some links that will help you get familiar with the VS Code extension for Ada & SPARK:

* [Ada & SPARK for VS Code](integration/vscode/ada/README.md).
* [Tutorial: Using Ada in VS Code](https://github.com/AdaCore/ada_language_server/wiki/Getting-Started).

### Configuration

You can configure the extension via the `.vscode/settings.json` workspace settings file or the [multi-root workspace file](https://code.visualstudio.com/docs/editor/multi-root-workspaces).
See the setting list [here](doc/settings.md).

Here is an example config file:

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

### Refactoring

See a [dedicated document](doc/refactoring_tools.md) with the list of available refactorings.

### Tasks

The extension provides the following auto-detected tasks
(under `/Terminal/Run Task...` menu):

* `ada: Build current project` - launch `gprbuild` to build the current GPR project
* `ada: Check current file` - launch `gprbuild` to check errors in the current editor
* `ada: Clean current project` - launch `gprclean` to clean the current GPR project
* `spark: Examine project` - launch `gnatprove` in flow analysis mode on the current GPR project
* `spark: Examine file` - launch `gnatprove` in flow analysis mode on the file in the current editor
* `spark: Examine subprogram` - launch `gnatprove` in flow analysis mode on the current subprogram in the current editor
* `spark: Prove project` - launch `gnatprove` on the current GPR project
* `spark: Prove file` - launch `gnatprove` on the file in the current editor
* `spark: Prove subprogram` - launch `gnatprove` on the current subprogram in the current editor
* `spark: Prove selected region` - launch `gnatprove` on the selected region in the current editor
* `spark: Prove line` - launch `gnatprove` on the cursor line in the current editor
* `spark: Clean project for proof` - launch `gnatprove` on the current GPR project to clean proof artefacts

You can bind keyboard shortcuts to them by adding to the `keybindings.json` file:

```json
{
  "key": "alt+v",
  "command": "workbench.action.tasks.runTask",
  "args": "ada: Check current file",
  "when": "editorLangId == ada"
}
```

#### Task Customization

You can [customize auto-detected tasks](https://code.visualstudio.com/docs/editor/tasks#_customizing-autodetected-tasks)
by providing extra tool command line options via the `args` property of the `configuration` object in the `tasks.json`:

```json
{
   "version": "2.0.0",
   "tasks": [
      {
         "type": "ada",
         "configuration": {
            "kind": "buildProject",
            "projectFile": "${config:ada.projectFile}",
            "args": ["-gargs", "-vh"]
         },
         "problemMatcher": ["$ada"],
         "group": "build",
         "label": "ada: Build current project with custom options"
      }
   ]
}
```

### Commands and Shortcuts

The extension contributes commands and a few default key bindings.
Below are a few examples, and other commands can be found by searching for `Ada:` in the command list.

#### Ada: Go to other file

This command switches between specification and implementation Ada files.
The default shortcut is `Alt+O`.

#### Ada: Add subprogram box

This command inserts a comment box before the current subprogram body.
The default shortcut is `Alt+Shift+B`.

#### Ada: Reload project

This command reloads the current project.
The default shortcut is `None`.

### Bug Reporting

You can use the VS Code `Issue Reporter` to report issues. Just click on the `Help -> Report Issue` menu, select `An extension` for the `File on` entry and `Language Support for Ada` for the extension name. Put as many information you can in the description, like steps to reproduce, stacktraces or system information (VS Code automatically includes it by default). This will create a GitHub issue in the [Ada Language Server](https://github.com/AdaCore/ada_language_server/) repository.

ALS log files can be found under the `~/.als` directory (`%USERPROFILE%/.als` on Windows). Feel free to attach them on the issues, it helps a lot for further investigation, specially when the `ALS.IN` and `ALS.OUT` traces are enabled (more info about traces configuration can be found [here](doc/traces.md).)

### Limitations and Differences with GNAT Studio

The VS Code extension has a few limitations and some differences compared to [GNAT Studio](https://github.com/AdaCore/gnatstudio):

* **Indentation/formatting**: it does not support automatic indentation when adding a newline and range/document
formatting might no succeed on incomplete/illegal code.

* **Tooling support**: we currently provide minimal support for *SPARK* (see *Prove/Examine* tasks in the [Auto-detected tasks](#auto-detected-tasks) section), but there is no support for tools such as *CodePeer*, *GNATcheck*, *GNATtest* or *GNATcoverage*.

* **Alire support**: if the root folder contains an `alire.toml` file and
  there is `alr` executable in the `PATH`, then the language server fetches
  the project's search path, environment variables and the project's file
  name from the crate description.

* **Project support**: there is no `Scenario` view: users should configure scenarios via the *ada.scenarioVariables* setting (see the settings list available [here](doc/settings.md)). Saving the settings file after changing the values will automatically reload the project and update the
predefined tasks to take into account the new scenario values.

  Source directories from imported projects should be added in a [workspace file](https://code.visualstudio.com/docs/editor/workspaces#_multiroot-workspaces). If you already have a workspace file, the extension will propose you to automatically add all the source directories coming from imported projects to your workspace automatically at startup.

## Integration with other editors and IDEs

### Integration with Coc.NVim

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

### Integration with vim-lsp

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

### Integration with LanguageClient-Neovim

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

### Integration with Neovim's built-in LSP client

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

### Integration with QtCreator

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
