# Ada Language Support

[![Build Status](https://travis-ci.org/AdaCore/ada_language_server.svg?branch=master)](https://travis-ci.org/AdaCore/ada_language_server)

This extension provides support for Ada programing language via
[Ada language server](https://github.com/AdaCore/ada_language_server)
which utilizes
[Libadalang library](https://github.com/AdaCore/libadalang).

## Features

 * [GNAT project files](https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/gnat_project_manager.html)
 * Code completion
 * Go to definition, corresponding references
 * Document symbol search

## Requirements

To enable cross-references GNAT compiler should be installed.

## Extension Settings

This extension contributes the following settings:

 * Use `ada.projectFile` to configure the GNAT Project File
 * Use `ada.scenarioVariables` to configure scenario variables.

Here is an example config file:

```json
{
    "ada.projectFile": "my_project.gpr",
    "ada.scenarioVariables": {
        "BUILD_DIR": "build",
        "OS": "Windows_NT"
    }
}
```

## Feedback and Known Issues

File a bug or see known issues [at github](https://github.com/AdaCore/ada_language_server/issues/).

## Release Notes

### 0.0.1

Initial release of Ada Language Support extension

----

## License
GPL 3.0, See [LICENSE](LICENSE) for more information.
