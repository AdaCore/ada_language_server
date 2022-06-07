# Ada/SPARK for Visual Studio Code

#### [Repository](https://github.com/AdaCore/ada_language_server)&nbsp;&nbsp;|&nbsp;&nbsp;[Issues](https://github.com/AdaCore/ada_language_server/issues)&nbsp;&nbsp;|&nbsp;&nbsp;[Documentation](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/ada/README.md#documentation)&nbsp;&nbsp;|&nbsp;&nbsp;[Code Samples](https://github.com/AdaCore/ada_language_server/tree/master/integration/vscode/Code%20Samples)&nbsp;&nbsp;|&nbsp;&nbsp;[Tutorial](https://github.com/AdaCore/ada_language_server/wiki/Getting-Started)


[![Build binaries](https://github.com/AdaCore/ada_language_server/workflows/Build%20binaries/badge.svg)](https://github.com/AdaCore/ada_language_server/actions)
[![GitHub tag (latest by date)](https://img.shields.io/github/v/tag/AdaCore/ada_language_server)](https://github.com/AdaCore/ada_language_server/releases)
[![VS Marketplace](https://img.shields.io/visual-studio-marketplace/v/adacore.ada?label=VS%20Marketplace)](https://marketplace.visualstudio.com/items?itemName=AdaCore.ada)
[![Open VSX Registry](https://img.shields.io/open-vsx/v/AdaCore/ada?label=Open%20VSX)](https://open-vsx.org/extension/AdaCore/ada)
[![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-blue?logo=gitpod)](https://gitpod.io/#https://github.com/AdaCore/ada_language_server/tree/edge)

This extension provides support for Ada/SPARK programing language via
[Ada language server](https://github.com/AdaCore/ada_language_server)
which utilizes
[Libadalang library](https://github.com/AdaCore/libadalang).

## Getting started

### Ada compiler and debugger
The extension does not include an Ada compiler nor debugger. You will need to install these tools or use those already installed on your computer.

Popular Ada compilers are:
 * [GNAT Community Edition](https://www.adacore.com/download/more)
 * GNAT of [GCC](https://gcc.gnu.org/) in a Linux distro. Also install `gprbuild` package.
 * GNAT for Windows in [msys2](https://www.msys2.org/). Also install `gprbuild` package.
 * [GNAT for Mac OS X](https://sourceforge.net/projects/gnuada/files/GNAT_GCC%20Mac%20OS%20X/)

Make sure your compiler executable is in your platform path so the extension can find it. You can check availability of your Ada tools by opening the Integrated Terminal (Ctrl+\`) in VS Code and try running the executable (for example `gnatls -v`).

### Install the Language Support for Ada extension

Launch VS Code Quick Open (Ctrl+P), paste the following command, and press enter.

    ext install AdaCore.ada

## Documentation

Some useful links:
 * [Tutorial: Using Ada in VS Code](https://github.com/AdaCore/ada_language_server/wiki/Getting-Started)
 * [Supported refactoring tools](https://github.com/AdaCore/ada_language_server/blob/master/doc/refactoring_tools.md)
 * [Custom colors for Ada sources](https://github.com/AdaCore/ada_language_server/wiki/Custom-colors-in-VS-Code)
 * [Usage in a Remote container](https://github.com/AdaCore/ada_language_server/tree/master/integration/vscode/Code%20Samples/docker) example
 * Ada Language Server [README.md](https://github.com/AdaCore/ada_language_server/blob/master/README.md)
 * [Code Samples](https://github.com/AdaCore/ada_language_server/tree/master/integration/vscode/Code%20Samples) with Build and Debug tasks


## Feedback and Known Issues

File a bug or see known issues [at github](https://github.com/AdaCore/ada_language_server/issues/).

