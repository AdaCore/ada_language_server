# ALS Trace File

Default trace file name is `$HOME/.als/traces.cfg`. This file gets automatically created
if not present on the disk. The first line of the traces file
defines the traces output stream (a filename in our case) and the other
lines are used to enable or disable traces.

Note that you can provide another traces file via the `--tracefile=<FILE>` command line option.

Here is a list of the most useful supported traces:

## `ALS.IN` (default no)
Shows all the server's input. Use this way:

    ALS.IN=yes

## `ALS.OUT` (default no)
Shows all the server's output. Use this way:

    ALS.OUT=yes

## `ALS.MAIN` (default yes)
Trace requests, notifications and responses in ALS log files. Will
also log any exception that occurs when handling LSP requests.

    ALS.MAIN=yes

## `ALS.ALLOW_INCREMENTAL_TEXT_CHANGES` (default yes)
Request incremental (if `yes`) or full content (if `no`)
[text document syncronization](https://microsoft.github.io/language-server-protocol/specification#textDocument_synchronization)
in `initialize` response.

    ALS.ALLOW_INCREMENTAL_TEXT_CHANGES=no

## `ALS.LAL_PP_OUTPUT_ON_FORMATTING` (default no)
Log lalpp output if `yes`.

    ALS.LAL_PP_OUTPUT_ON_FORMATTING=yes
