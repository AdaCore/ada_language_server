
# Set workspace-specific environment variables

This small tutorial will help you to create a workspace-specific environment, allowing you to set and/or modify environment variables for your project (e.g: put the needed toolchain in front of your `PATH`).

This is particularly useful when working on [remote machines](Working-on-a-remote-machine) in order
to avoid modifying the environment globally just to work on a given project.

## Prerequisites

To successfully complete this tutorial, you must do the following:

* Install [Visual Studio Code](https://code.visualstudio.com/download).

* Install the
[Ada extension for VS Code](https://marketplace.visualstudio.com/items?itemName=AdaCore.ada).
You can install the Ada extension by searching for 'adacore' in the
Extensions view (`Ctrl+Shift+X`).

* Have a an installed GNAT toolchain for your platform (see [here](Getting-Started#ensure-gnat-is-installed) for more information on how to install it).

## How to set environment variables

Basically you just need to specify your environment variables and their associated values via the `terminal.integrated.env.*` VS Code settings in your workspace file (or your `settings.json` file), like in the following example:

```json
 // Set a workspace-specific environment for OSX platforms.
 "terminal.integrated.env.osx": {
         //  Set MAIN_NUMBER scenario variable to MAIN_2 directly from the environment
  "MAIN_NUMBER": "MAIN_2",

                //  Set custom GPR_PROJECT_PATH
  "GPR_PROJECT_PATH": "${workspaceFolder}/imported:${env:GPR_PROJECT_PATH}:"
        },

 // Set a workspace-specific environment for Linux platforms.
 "terminal.integrated.env.linux": {
         //  Set MAIN_NUMBER scenario variable to MAIN_2 directly from the environment
  "MAIN_NUMBER": "MAIN_2",

                //  Set custom GPR_PROJECT_PATH
  "GPR_PROJECT_PATH": "${workspaceFolder}/imported:${env:GPR_PROJECT_PATH}:"
        },

 // Set a workspace-specific environment for Windows
 "terminal.integrated.env.windows": {
  //  Set MAIN_NUMBER scenario variable to MAIN_2 directly from the environment
  "MAIN_NUMBER": "MAIN_2",

                //  Set custom GPR_PROJECT_PATH
  "GPR_PROJECT_PATH": "${workspaceFolder}\\imported;${env:GPR_PROJECT_PATH}:"
 }
```

## Example

You can check the [Custom Env](https://github.com/AdaCore/ada_language_server/tree/master/integration/vscode/Code%20Samples/custom_env) code sample in this repository to have an example of workspace that sets a custom environment.
