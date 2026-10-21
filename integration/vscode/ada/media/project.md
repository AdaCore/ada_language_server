Start with the [hello crate](https://alire.ada.dev/crates/hello), a Hello World application that uses the `libhello` library from Alire.

Click **Get the Alire Hello World Project** and choose a parent folder. The extension runs `alr get --build hello` in a task terminal. Follow any Alire prompts in that terminal, including toolchain selection if you have not already selected one. This downloads the example and its dependencies and builds it.

When the build succeeds, the extension opens the downloaded `hello_<version>_<hash>` folder as your workspace and restores this walkthrough. Existing folders are left untouched. If the download or build fails, check the task terminal for details.

Explore these files:

- `alire.toml` describes the crate and its dependencies.
- `hello.gpr` specifies source directories, build options, and the application's main program.
- `src/hello.adb` contains the main program. Try editing it before building again in the next steps.

For an existing Alire project, open the folder containing `alire.toml`. The extension automatically loads the project using Alire. For projects with several GPR files, use [Ada: Open Project...](command:ada.openProjectFile) to select the main project explicitly.

To start your own application instead, run `alr init --bin my_project`, then `cd my_project` and `alr build`, and open that folder in VS Code.

📖 Learn more about project files in the [GPRbuild and GPR Companion Tools User’s Guide](https://docs.adacore.com/live/wave/gprbuild/html/gprbuild_ug/gprbuild_ug.html).
