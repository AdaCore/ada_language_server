If your project defines a `Main` entry point, you can launch or debug it as follows.

# 🚀 Launch

- The [`Tasks: Run Task`](command:toSide:workbench.action.tasks.runTask) command offers tasks called `ada: Run main - ...` and `ada: Build and run main - ...`. The former run the main executable while the latter conveniently builds the project and runs the executable at once.
- A Run button is displayed right above the declaration of the main subprogram. It's a shortcut to the `ada: Build and run main - ...` task.

To provide command line argument to your executable, use the gear icon at the right-hand side of the task to customize arguments in the `tasks.json` file.

Alternatively, open a new terminal and launch the executable manually with the desired command line arguments.

For the Alire Hello World example, you can also run `alr run` in the crate's folder to build and launch it, or `alr run -- arg1 arg2` to pass arguments.

# 🐞 Debug

Ada debugging leverages the integration of GDB through the [Microsoft C/C++](command:toSide:extension.open?%22ms-vscode.cpptools%22) VS Code extension.

You can start debugging by running the [`Debug: Start Debugging`](command:toSide:ada.walkthroughStartDebugging) command and selecting `Ada` if multiple languages are offered.

The extension generates the debug configuration from the project's `Main` entry points. No `launch.json` file is needed to debug the Hello World example.

For the Hello World example, the walkthrough sets a breakpoint in `src/hello.adb`. The native GNAT toolchain downloaded by Alire includes GDB, which the extension finds automatically in the crate's Alire environment. No separate GDB installation is needed for this walkthrough.

If you use a different toolchain that does not include GDB, install GDB for your platform and make it available on `PATH`.

If you would like to provide command line arguments to the debugged program or customize GDB's behavior, use the [`Debug: Add Configuration`](command:toSide:debug.addConfiguration) command to create a `launch.json` where you can customize these aspects.
