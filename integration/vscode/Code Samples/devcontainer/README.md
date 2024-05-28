# Workspace-specific environment for devcontainers

This demo presents a workspace-specific environment specification for
devcontainers. This is a modified version of `custom_env` example.

The `.devcontainer` folder contains a `devcontainer.json` file with
instructions on how to build the container, install Alire and
install the Ada toolchain with Alire.

The folder contains a `custom_env.code-workspace` file that setups a custom
environment for this workspace, setting and modifying some environment
variables, including the `PATH` pointing to the Alire installed toolchain.

NOTE! Please check `PATH` variable in the `custom_env.code-workspace` file,
because it will outdate as Alire gets updated versions of the toolchain.

# Run the Code Sample

Open VS Code and click on the `File -> Open Workspace From File...` menu
and select the `custom_env.code-workspace` file present in this folder.

VS Code propose to build the devcontainer and open the folder in it.
As you agree, container will be built and GNAT toolchain will be installed.
After that you should reload the project (with `Ada: repoad project` command
launched from the Command palette `Ctrl+Shift+P`) or reload the window
(with `Developer: Reload Window` command).

Now you have ready to use environment!

You can then build the project by clicking on the `Terminal -> Run Task...` menu,
and by selecting the `ada` then `ada: Build current project` task.
