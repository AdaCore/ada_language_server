Ada and SPARK are compiled languages which means that a compiler is needed to translate the source code into a program that can be executed.

For community users, we recommend [Alire](https://alire.ada.dev/), the package manager for Ada and SPARK. It downloads compiler toolchains and manages your project's library dependencies.

1. [Download and install Alire](https://alire.ada.dev/docs/#installation) for your platform.
2. Make the `alr` command available on the `PATH` used to start VS Code. Restart VS Code after changing your environment. On Windows, you can start VS Code from the Alire PowerShell shortcut.
3. In a terminal, run `alr toolchain --select` and select the native GNAT compiler (`gnat_native`) and GPRbuild. Alire downloads the selected tools. Accept any platform setup prompts needed by Alire.

The next step downloads an example Alire project. The extension uses its Alire environment automatically, so you do not need to add the downloaded compiler's directory to `PATH` separately.

Already have an AdaCore subscription? You can use your [GNAT Pro tools](https://github.com/AdaCore/ada_language_server/tree/master/integration/vscode/ada#getting-additional-tools) with Alire by selecting the tools available in your environment.
