Use [Alire](https://alire.ada.dev/), the Ada and SPARK package manager, to install a compiler and manage project dependencies.

1. Click **Install Alire** to use an existing installation or install it in a folder of your choice (default: `~/alire`).
2. In the terminal that opens, select the native GNAT compiler (`gnat_native`) and GPRbuild. Accept any setup prompts, including MSYS2 on Windows.
3. Continue with **Get an Alire project** to download the Hello World example.

The native GNAT toolchain downloaded by Alire includes GDB. The extension finds it automatically when debugging an Alire project, so no separate GDB installation is needed for this walkthrough.

Alire is available in new VS Code terminals after setup. To use it elsewhere, follow **Use Alire outside VS Code** in the completion message.

For [manual installation](https://alire.ada.dev/docs/#installation), set `ada.alire.executablePath` or add the directory containing the `alr` executable to `PATH`, then restart VS Code. Windows ARM64 requires Windows 11 for automatic installation; on Windows 10, use a compatible manual installation or WSL.

Already have an AdaCore subscription? You can use your [GNAT Pro tools](https://github.com/AdaCore/ada_language_server/tree/master/integration/vscode/ada#getting-additional-tools) with Alire by selecting the tools available in your environment.
