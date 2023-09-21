# 🔨 Build a project

Use "auto-detected tasks" (i.e: `gnat` tasks in the `Run Task` dialog) to
 * ⚙ Build the whole project
 * ✅ Check errors in the current file
 * 🔎 Launch `gnatprove` to verify your SPARK code, if any (you can find the tool [here](https://github.com/alire-project/GNAT-FSF-builds/releases))

## 🚀 Launch

Open a new terminal to launch your executable once it has been built.

## 🐞 Debug

The [ms-vscode.cpptools](https://github.com/microsoft/vscode-cpptools) extension
is automatically installed along with this extension, allowing to use its
integration for GDB to debug Ada code.

The extension provides default debug configurations for all the mains of your
project. You can debug the executable of your choice by opening the
`Run and Debug` panel and then by clicking on the `Run and Debug` button.
You can also run directly the `Debug: Start Debugging` command instead of using the UI. In some circumstances the UI may offer a selection of languages, in which case select `Ada`.
The extension will then propose you to select the main you want to debug if your project contains several mains.

If you want to customize GDB's behavior or if you need to add custom arguments, you
will have to create a `launch.json` file: this can be done through the
`create a launch.json file` link button in the `Run and Debug` panel or directly via
the `Debug: Add Configuration` command.
