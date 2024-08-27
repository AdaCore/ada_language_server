Several VS Code tasks are provided to work with Ada & SPARK projects.
You can find them by using the [`Tasks: Run Task`](command:workbench.action.tasks.runTask) command and selecting the `ada` or `spark` task groups.

Use the [`ada: Build current project`](command:workbench.action.tasks.runTask?%22ada%3A%20Build%20current%20project%22) task to build your project.

If you would like to customize a task, for example by providing additional command line arguments, you can use the gear icon on the right-hand side of a task and edit it in your workspace's `tasks.json` file.

The [Alire](https://alire.ada.dev/) package manager for Ada and SPARK is also supported seamlessly. If it is available in the environment and if your project is an Alire crate, then all tasks will automatically use Alire.
