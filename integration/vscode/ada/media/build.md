Several VS Code tasks are provided to work with Ada & SPARK projects.
You can find them by using the [`Tasks: Run Task`](command:workbench.action.tasks.runTask) command and selecting the `ada` or `spark` task groups.

Use the [`ada: Build current project`](command:workbench.action.tasks.runTask?%22ada%3A%20Build%20current%20project%22) task to build your project.

For an Alire project, this task runs `alr build`. Alire selects the compiler and build tools, fetches library dependencies, and sets up the build environment. You can also run `alr build` yourself in a terminal opened in the crate's folder.

Once the build completes, address any compiler warnings shown in the [Problems](command:workbench.actions.view.problems) panel.

If you would like to customize a task, for example by providing additional command line arguments, you can use the gear icon on the right-hand side of a task and edit it in your workspace's `tasks.json` file.

Other Ada and SPARK tasks also use the [Alire](https://alire.ada.dev/) environment when your workspace contains an `alire.toml` file and `alr` is available. Individual tasks may require additional tools.
