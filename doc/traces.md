# ALS Trace File

Default trace file name is `$HOME/.als/traces.cfg`.
You can provide another file by `--tracefile=<FILE>` command line option.

Here is a list of supported settings.

## `ALS.IN` (default no)
Show all the server input. Use this way:

    ALS.IN=yes > inout.txt:buffer_size=0

## `ALS.OUT` (default no)
Show all the server output. Use this way:

    ALS.OUT=yes > inout.txt:buffer_size=0

## `ALS.MAIN` (default no)
Trace requests, notifications and responses in an ALS log file.

    ALS.MAIN=yes

## `ALS.ALLOW_INCREMENTAL_TEXT_CHANGES` (default yes)
Request incremental (if `yes`) or full content (if `no`)
[text document syncronization](https://microsoft.github.io/language-server-protocol/specification#textDocument_synchronization)
in `initialize` response.

    ALS.ALLOW_INCREMENTAL_TEXT_CHANGES=no

## `ALS.LAL_PP_OUTPUT_ON_FORMATTING` (default no)
Log lalpp output if `yes`.

    ALS.LAL_PP_OUTPUT_ON_FORMATTING=yes
