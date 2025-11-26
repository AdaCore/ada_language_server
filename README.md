
# Ada Language Server

[![Build binaries](https://github.com/AdaCore/ada_language_server/workflows/Build%20binaries/badge.svg)](https://github.com/AdaCore/ada_language_server/actions)
[![GitHub tag (latest by date)](https://img.shields.io/github/v/tag/AdaCore/ada_language_server)](https://github.com/AdaCore/ada_language_server/releases)
[![VS Marketplace](https://img.shields.io/visual-studio-marketplace/v/adacore.ada?label=VS%20Marketplace)](https://marketplace.visualstudio.com/items?itemName=AdaCore.ada)
[![Open VSX Registry](https://img.shields.io/open-vsx/v/AdaCore/ada?label=Open%20VSX)](https://open-vsx.org/extension/AdaCore/ada)
[![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-blue?logo=gitpod)](https://gitpod.io/#https://github.com/AdaCore/ada_language_server/)

This repository contains an implementation of the [Microsoft Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
for Ada/SPARK and GPR project files.

Current features (general):

* [GNAT project files](https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/gnat_project_manager.html) support.
* Basic [Alire](https://alire.ada.dev/) support.

For Ada/SPARK, we provide the following:

* Code completion for names, keywords, aggregates, etc.
* Code navigation, such as Go to Definition/Declaration, Find All References, Call Hierarchies, etc.
* [Code refactoring](#refactoring-tools) like insert named associations, auto-add `with`-clauses, etc.
* Document/Workspace symbol search.
* Code folding and formatting.

The Ada Language Server now also supports the GPR language, via the
`--language-gpr` option, providing support for the most used LSP features
such as navigation, outline and tooltips for GPR files. When this switch is
present, the server will only support GPR files. To support both GPR and
Ada/SPARK, you'll need to launch two instances of the server.
You can refer to the [Supported LSP Server Requests](#supported-lsp-server-requests)
section for more information.

We also provide [Visual Studio Code](https://code.visualstudio.com/)
extension at
[the VS Marketplace](https://marketplace.visualstudio.com/items?itemName=AdaCore.ada) and at
[the Open VSX Registry](https://open-vsx.org/extension/AdaCore/ada).

## Table of Contents

- [Ada Language Server](#ada-language-server)
  - [Table of Contents](#table-of-contents)
  - [Install](#install)
  - [Usage](#usage)
    - [Memory Consumption](#memory-consumption)
  - [Supported LSP Server Requests](#supported-lsp-server-requests)
    - [Protocol Extensions](#protocol-extensions)
  - [VS Code Extension](#vs-code-extension)
  - [Integration with other editors and IDEs](#integration-with-other-editors-and-ides)
    - [Integration with Coc.NVim](#integration-with-cocnvim)
    - [Integration with vim-lsp](#integration-with-vim-lsp)
    - [Integration with LanguageClient-Neovim](#integration-with-languageclient-neovim)
    - [Integration with Neovim's built-in LSP client](#integration-with-neovims-built-in-lsp-client)
    - [Integration with emacs lsp-mode](#integration-with-emacs-lsp-mode)
    - [Integration with QtCreator](#integration-with-qtcreator)
    - [Integration with IntelliJ](#integration-with-intellij)
  - [Refactoring Tools](#refactoring-tools)
  - [Authors \& Contributors](#authors--contributors)
  - [Contribute](#contribute)
  - [License](#license)

## Install

The official [releases][releases] contains ready-built packages for
various platforms and architectures and should be your first choice when trying out the language server.

You can also build the language server from source. See [build.md](doc/devel/build.md) for details.

[releases]: https://github.com/AdaCore/ada_language_server/releases

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

### Memory Consumption

The `ada_language_server` relies on [Libadalang](https://github.com/AdaCore/libadalang) to compute the cross references.
Most of this computation is done while indexing which will create an internal cache.
The expected memory size of this cache is around 300Mb per 100k lines of Ada code.
Furthermore, 450Mb are necessary for the runtime.
Please note that some Ada structures like generics and tagged types might
increase the memory usage. This is also the case when using aggregate projects.
These measures were taken using both Resident Set Size and [Valgrind massif](https://valgrind.org/docs/manual/ms-manual.html) on Ubuntu 22.04LTS.

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

See the [Ada & SPARK VS Code Extension User's Guide](doc/vscode-ug.md) for more information.

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

Alternatively to the above settings section, workspace-specific [ALS Settings](doc/settings.md) such as the `projectFile` can be provided in a `.als.json` file at the root of the workspace.

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
        \ })
endif
```

Workspace-specific [ALS Settings](doc/settings.md) such as the `projectFile` can be provided in a `.als.json` file at the root of the workspace.

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

Workspace-specific [ALS Settings](doc/settings.md) such as the `projectFile` can be provided in a `.als.json` file at the root of the workspace.

### Integration with Neovim's built-in LSP client

Neovim 0.5.0 and later have a built-in LSP client which can be used with the
Ada Language Server. In order to use it with minimal effort, follow these steps:

* Install the ada language server and make sure it's in your $PATH.
* Use your favorite Neovim plugin manager to add the default set of [LSP
  configuration files](https://github.com/neovim/nvim-lspconfig) to Neovim.
* Add `require('lspconfig').ada_ls.setup{}` to your init.lua in order to enable
  the Ada Language Server.

If you would rather not have the ada language server in your path, you can give
the lsp client an absolute path to the ALS executable:

```lua
require('lspconfig').ada_ls.setup{ cmd = "/path/to/als/executable" }
```

Configuring the language server's settings globally can be achieved like this:

```lua
require('lspconfig').ada_ls.setup{
  settings = {
    ada = {
      projectFile = "project.gpr";
      scenarioVariables = { ... };
    }
  }
}
```

Workspace-specific [ALS Settings](doc/settings.md) such as the `projectFile` can be provided in a `.als.json` file at the root of the workspace.

Alternatively, workspace-specific settings can also be configured as per the
[lspconfig wiki](https://github.com/neovim/nvim-lspconfig/wiki/Project-local-settings)

### Integration with emacs lsp-mode

Workspace-specific [ALS Settings](doc/settings.md) such as the `projectFile` can be provided in a `.als.json` file at the root of the workspace.

Alternatively the configuration for each project can be provided using a `.dir-locals.el`
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
the Ada Language Server.

Workspace-specific [ALS Settings](doc/settings.md) such as the `projectFile` can be provided in a `.als.json` file at the root of the workspace.

### Integration with IntelliJ

The [LSP4IJ](https://plugins.jetbrains.com/plugin/23257-lsp4ij) IntelliJ plugin
provides a template for Ada since version `0.14.0`, allowing users tu use the Ada Language Server from IntelliJ.

Follow the [dedicated LSP4J documentation](https://github.com/redhat-developer/lsp4ij/blob/main/docs/user-defined-ls/ada_language_server.md) for more information.

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
