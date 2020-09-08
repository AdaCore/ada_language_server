# Ada/SPARK for Visual Studio Code

#### [Repository](https://github.com/AdaCore/ada_language_server)&nbsp;&nbsp;|&nbsp;&nbsp;[Issues](https://github.com/AdaCore/ada_language_server/issues)&nbsp;&nbsp;|&nbsp;&nbsp;[Documentation](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/ada/README.md)&nbsp;&nbsp;|&nbsp;&nbsp;[Code Samples](https://github.com/AdaCore/ada_language_server/tree/master/integration/vscode/Code%20Samples)&nbsp;&nbsp;|&nbsp;&nbsp;[Offline Installers](https://bintray.com/reznikmm/ada-language-server/ada-language-server/_latestVersion)


[![Build Status](https://travis-ci.org/AdaCore/ada_language_server.svg?branch=master)](https://travis-ci.org/AdaCore/ada_language_server)

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
 * Ada Language Server [README.md](https://github.com/AdaCore/ada_language_server/blob/master/README.md)
 * [Code Samples](https://github.com/AdaCore/ada_language_server/tree/master/integration/vscode/Code%20Samples) with Build and Debug tasks


## Feedback and Known Issues

File a bug or see known issues [at github](https://github.com/AdaCore/ada_language_server/issues/).

