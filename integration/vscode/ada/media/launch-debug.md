If your project defines a `Main` entry point, you can launch or debug it as follows.

# 🚀 Launch

- The [`Tasks: Run Task`](command:toSide:workbench.action.tasks.runTask) command offers tasks called `ada: Run main - ...` and `ada: Build and run main - ...`. The former run the main executable while the latter conveniently builds the project and runs the executable at once.
- A Run button is displayed right above the declaration of the main subprogram. It's a shortcut to the `ada: Build and run main - ...` task.

To provide command line argument to your executable, use the gear icon at the right-hand side of the task to customize arguments in the `tasks.json` file.

Alternatively, open a new terminal and launch the executable manually with the desired command line arguments.

# 🐞 Debug

Ada debugging leverages the integration of GDB through the [Microsoft C/C++](command:toSide:extension.open?%22ms-vscode.cpptools%22) VS Code extension.

You can start debugging by running the [`Debug: Start Debugging`](command:toSide:ada.walkthroughStartDebugging) command and selecting `Ada` if multiple languages are offered.

If you would like to provide command line arguments to the debugged program or customize GDB's behavior, use the [`Debug: Add Configuration`](command:toSide:debug.addConfiguration) command to create a `launch.json` where you can customize these aspects.
