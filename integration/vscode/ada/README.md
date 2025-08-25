# Ada & SPARK for Visual Studio Code

<!-- markdownlint-disable MD001 -->
#### [Repository](https://github.com/AdaCore/ada_language_server)&nbsp;&nbsp;|&nbsp;&nbsp;[Issues](https://github.com/AdaCore/ada_language_server/issues)&nbsp;&nbsp;|&nbsp;&nbsp;[Documentation](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/ada/README.md#documentation)&nbsp;&nbsp;|&nbsp;&nbsp;[Code Samples](https://github.com/AdaCore/ada_language_server/tree/master/integration/vscode/Code%20Samples)&nbsp;&nbsp;|&nbsp;&nbsp;[Tutorial](https://github.com/AdaCore/ada_language_server/wiki/Getting-Started)

[![Build binaries](https://github.com/AdaCore/ada_language_server/workflows/Build%20binaries/badge.svg)](https://github.com/AdaCore/ada_language_server/actions)
[![GitHub tag (latest by date)](https://img.shields.io/github/v/tag/AdaCore/ada_language_server)](https://github.com/AdaCore/ada_language_server/releases)
[![VS Marketplace](https://img.shields.io/visual-studio-marketplace/v/adacore.ada?label=VS%20Marketplace)](https://marketplace.visualstudio.com/items?itemName=AdaCore.ada)
[![Open VSX Registry](https://img.shields.io/open-vsx/v/AdaCore/ada?label=Open%20VSX)](https://open-vsx.org/extension/AdaCore/ada)
[![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-blue?logo=gitpod)](https://gitpod.io/#https://github.com/AdaCore/ada_language_server/tree/edge)

This extension provides support for the Ada and SPARK programming languages in VS Code via the [Ada Language Server](https://github.com/AdaCore/ada_language_server) based on the [Libadalang library](https://github.com/AdaCore/libadalang).

## Features

Ada and SPARK are compiled languages which means that a compiler (GNAT) is needed to translate the source code into a program that can be executed.
Other tools are also needed to perform tasks such as testing, static analysis and formal proof of SPARK code.

This extension **does not include a compiler nor additional tools**. Nonetheless it offers a number of features out of the box and more capabilities can be accessed by installing additional tools.

<!-- markdownlint-disable MD033 -->
| Tool                                         | Feature | Support |
|----------------------------------------------|---|:-:|
| **Ada & SPARK Extension**                    |   | |
| | Syntax Highlighting                          | ✅ |
| | Navigation<br>(except standard runtime)      | ✅ |
| | Auto-completion<br>(except standard runtime) | ✅ |
| | Refactoring                                  | ✅ |
| **GNAT Compiler** | | |
| | Full Navigation      | ✅ |
| | Full Auto-completion | ✅ |
| | Build                | ✅ |
| | Debug                | ✅ |
| **GNAT DAS** | | |
| | Test                | ✅ |
| | Code Coverage       | ✅ |
| **GNAT SAS** | | |
| | Static Analysis     |       ✅            |
| **SPARK** | | |
| | Formal Proof        |       ✅          |

## Getting Additional Tools

For a fully operational development environment you can obtain a compiler and/or other tools from the following channels.

### AdaCore Customers

If you are an [AdaCore](https://www.adacore.com/) customer, log into your account on [GNAT Tracker](https://support.adacore.com/csm) to download the tools available in your subscription.

### Community Users

The Ada & SPARK tools are available to the community through different channels:

* [ALIRE](https://alire.ada.dev/): The Ada LIbrary Repository provides the means to install compiler [toolchains](https://alire.ada.dev/docs/#toolchain-management).
The `gnatprove` crate provides GNATprove.
Both tools are available for Linux, Windows and macOS.
* On Windows with [msys2](https://www.msys2.org/) you can install the `gcc` and `gprbuild` packages.
* On macOS you can find GCC releases including GNAT for Intel and Apple silicon at [this project on GitHub](https://github.com/simonjwright/distributing-gcc/releases) courtesy of Simon Wright.

## Environment Setup

The following environment variables influence the operation of the Ada extension:

* `PATH` should include the path to the GNAT compiler installation in order to benefit from auto-completion and navigation into the standard runtime.
Without it, auto-completion and navigation will work only on the sources visible in the project closure, but not on the packages of the standard library `Ada.*`.

* `GPR_PROJECT_PATH` provides paths to other `.gpr` Ada projects that your project depends on.

When running VS Code locally, you can provide these environment variables by exporting them in a terminal, and starting VS Code from that same terminal with the `code` command.

## Documentation

* [Ada Language Server README.md: VS Code Extension](https://github.com/AdaCore/ada_language_server/blob/master/README.md#vs-code-extension)
* [Tutorial: Using Ada in VS Code](https://github.com/AdaCore/ada_language_server/wiki/Getting-Started)
* [Supported refactoring tools](https://github.com/AdaCore/ada_language_server/blob/master/doc/refactoring_tools.md)
* [Custom colors for Ada sources](https://github.com/AdaCore/ada_language_server/wiki/Custom-colors-in-VS-Code)
* [Usage in a Remote container](https://github.com/AdaCore/ada_language_server/tree/master/integration/vscode/Code%20Samples/docker) example
* [Code Samples](https://github.com/AdaCore/ada_language_server/tree/master/integration/vscode/Code%20Samples) with Build and Debug tasks

## Known Issues and Feedback

You can browse known issues and report bugs at the [Ada Language Server](https://github.com/AdaCore/ada_language_server/issues/) GitHub project.
