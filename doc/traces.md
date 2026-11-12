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

### Resetting to the default configuration

Existing traces configuration files are preserved when ALS is updated. To use
the current defaults, including after enabling verbose traces for troubleshooting:

1. Delete or rename `$HOME/.als/ada_ls_traces.cfg` and
   `$HOME/.als/gpr_ls_traces.cfg`. If `ALS_HOME` is set, use `$ALS_HOME/.als/`
   instead of `$HOME/.als/`.
2. Restart the language servers. In VS Code, run **Ada: Restart Language Servers**
   from the Command Palette.

Each server recreates its missing configuration file with the defaults when it
starts. These defaults enable `DEBUG.ABSOLUTE_TIME` and `DEBUG.ELAPSED_TIME` and
disable `ALS.IN` and `ALS.OUT`. Input and output tracing can significantly slow
down the servers and can include source code contents, which may be sensitive or
confidential. Review these logs before sharing them and reset the configuration
after collecting verbose logs.

If you launch ALS with `--tracefile=<FILE>`, remove that option to return to the
default configuration files.

## Supported Traces

Here is a list of the most useful supported traces:

### `ALS.IN` (default: no)

Shows all the server's input, which can include source code contents. These may
be sensitive or confidential; review the logs before sharing them. Use this way:

    ALS.IN=yes

### `ALS.OUT` (default: no)

Shows all the server's output, which can include source code contents. These may
be sensitive or confidential; review the logs before sharing them. Use this way:

    ALS.OUT=yes

### `ALS.MAIN` (default: yes)

Trace requests, notifications and responses without their parameters or results.
Entries show the direction, message name and numeric request ID when available.
String IDs are replaced with `<string>` because they may contain private data.
Incoming requests use their LSP method name; other messages use their Ada type
name. Outgoing messages are traced before serialization and transmission.

Exception entries show the operation and exception name. Source text, paths,
diagnostic details and full exceptions are available through `ALS.MAIN.DEBUG`.
Other trace channels, such as `ALS.PROJECT` and `ALS.CONFIG`, may still log paths
or configuration values.

    ALS.MAIN=yes

### `ALS.MAIN.DEBUG` (default: no)

Include detailed server diagnostics, full exceptions and raw messages that failed
to decode. These can contain source text, URIs, paths and other private data.
Review the logs before sharing them.

    ALS.MAIN.DEBUG=yes

### `DEBUG.ABSOLUTE_TIME` (default: yes)

Include a timestamp in each trace message.

    DEBUG.ABSOLUTE_TIME=yes

### `DEBUG.ELAPSED_TIME` (default: yes)

Include the elapsed time since the previous message from the same trace handle
to help investigate performance issues.

    DEBUG.ELAPSED_TIME=yes

### `ALS.ALLOW_INCREMENTAL_TEXT_CHANGES` (default: yes)

Request incremental (if `yes`) or full content (if `no`)
[text document syncronization](https://microsoft.github.io/language-server-protocol/specification#textDocument_synchronization)
in `initialize` response.

    ALS.ALLOW_INCREMENTAL_TEXT_CHANGES=no

### `ALS.LAL_PP_OUTPUT_ON_FORMATTING` (default: no)

Log lalpp output if `yes`.

    ALS.LAL_PP_OUTPUT_ON_FORMATTING=yes

## Troubleshooting

If the traces configuration file can't get parsed after customizing it (e.g: because it contains syntax errors), the Ada Language Server will use its own default traces configuration and log files will be produced in the directory from where the Ada Language Server is launched (i.e: the workspace's root directory in VS Code).

VS Code users can use the `Ada: Open Ada Language Server Log File` command to quickly open the  fallback log file in order understand why the Ada Language Server failed to parse the user's traces configuration file.
