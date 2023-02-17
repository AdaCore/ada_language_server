# 🔨 Build a project

Use "auto-detected tasks" to
 * ⚙ Build whole project
 * ✅ Check errors in the current file
 * 🔎 Lauch `gnatprove` to verify the code

## 🚀 Launch

Use embedded terminal to launch your executable.

## 🐞 Debug

The [ms-vscode.cpptools](https://github.com/microsoft/vscode-cpptools) extension
is automatically installed along with this extension, allowing to use its
integration for GDB to debug Ada code.

You will just need to add a proper debug configuration in your `launch.json`
file. Let's consider a basic `.gpr` project, which produces a
`main` executable in an `obj` directory:

```
project Default is

   for Main use ("main.adb");
   for Object_Dir use "obj";

end Default;

```

A basic `launch.json` file which allows to debug the produced `main`
executable would be:

```
{
  "name": "Ada Debugging",
  "type": "cppdbg",
  "request": "launch",
  "program": "${workspaceFolder}/obj/main",
  "cwd": "${workspaceFolder}"
}

```

You will then be able to go to the `Run and Debug` VS Code panel
in order to run the newly added debugger.

You can find more information about how to configure the debugger
[here](https://code.visualstudio.com/docs/cpp/launch-json-reference).