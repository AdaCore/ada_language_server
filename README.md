# ada_language_server

This repository contains a prototype implementation of the Microsoft Language
Server Protocol for Ada/SPARK

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
