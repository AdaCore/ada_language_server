# ALS Traces

## Traces Configuration Files

Default traces configuration file names are:

* For versions **< 24.0.6**:

   You will need to create a traces configuration file manually under `$HOME/.als/traces.cfg`. You can
   use the following contents as a starter:

   ```text
   >ada_ls_log.$T.$$.log:buffer_size=0
   ALS.MAIN=yes
   ALS.IN=no
   ALS.OUT=no
   ```

   This will produce `ada_ls_log.<timestamp>.log` log files for each Ada Language Server instance
   that gets spawned, either for Ada code or GPR files.

* For the **24.0.6** version:

   Default traces configuration file name is `$HOME/.als/traces.cfg`. This file gets automatically created if not present on the disk.

* For versions **> 24.0.6**:
   1. `$HOME/.als/ada_ls_traces.cfg` for the language server spawned for Ada code, which produce `ada_ls_log.<timestamp>.log` log files by default.
   2. `$HOME/.als/gpr_ls_traces.cfg` for the language server spawned for GPR files, which produce `gpr_ls_log.<timestamp>.log` log files by default.

   These files gets automatically created if not present on the disk.

The first line of the traces files defines the traces output stream (a filename in our case) and the other lines are used to enable or disable traces.

Note that you can provide another traces configuration file via the `--tracefile=<FILE>` command line option.

## Supported Traces

Here is a list of the most useful supported traces:

### `ALS.IN` (default: no)

Shows all the server's input. Use this way:

    ALS.IN=yes

### `ALS.OUT` (default: no)

Shows all the server's output. Use this way:

    ALS.OUT=yes

### `ALS.MAIN` (default: yes)

Trace requests, notifications and responses in ALS log files. Will
also log any exception that occurs when handling LSP requests.

    ALS.MAIN=yes

### `ALS.ALLOW_INCREMENTAL_TEXT_CHANGES` (default: yes)

Request incremental (if `yes`) or full content (if `no`)
[text document syncronization](https://microsoft.github.io/language-server-protocol/specification#textDocument_synchronization)
in `initialize` response.

    ALS.ALLOW_INCREMENTAL_TEXT_CHANGES=no

### `ALS.LAL_PP_OUTPUT_ON_FORMATTING` (default: no)

Log lalpp output if `yes`.

    ALS.LAL_PP_OUTPUT_ON_FORMATTING=yes

### `ALS.GNATFORMAT` (default: no)

Use GNATformat as format provider.

    ALS.GNATFORMAT=yes

## Troubleshooting

If the traces configuration file can't get parsed after customizing it (e.g: because it contains syntax errors), the Ada Language Server will use its own default traces configuration and log files will be produced in the directory from where the Ada Language Server is launched (i.e: the workspace's root directory in VS Code).

VS Code users can use the `Ada: Open Ada Language Server Log File` command to quickly open the  fallback log file in order understand why the Ada Language Server failed to parse the user's traces configuration file.
