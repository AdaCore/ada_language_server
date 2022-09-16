# Workspace-specific environment

This folder contains a `custom_env.code-workspace` file that setups a custom
environment for this workspace, setting and modifying some environment variables.

In particular it modifies `GPR_PROJECT_PATH` environment variable in order
to be able to build the `default.gpr` project, which includes a project
without specifying a path to it. You can find more information on the
`GPR_PROJECT_PATH` environment variable and on how to import other projects
[here](https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/gnat_project_manager.html?highlight=gpr_project_path#importing-projects).

It also sets the `MAIN_NUMBER` scenario variable to `MAIN_2` when loading
the workspace, instead of the default `MAIN_1` value.
More information on scenario variables is available [here](https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/gnat_project_manager.html?highlight=gpr_project_path#scenarios-in-projects).

Note that you can set and/or modify any environment variable, including the
`PATH` if you want to use a different Ada toolchain for this project.

# Run the Code Sample

Open VS Code and click on the `File -> Open Workspace From File...` menu
and select the `custom_env.code-workspace` file present in this folder.

You can then build the project by clicking on the `Terminal -> Run Task...` menu,
and by selecting the `gprbuild -> ada: Build current project` task.
