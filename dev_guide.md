# Ada language server

Language Servers for Ada and GPR files.

## Code structure

```sh
gnat/*.gpr   # project files - the main project for the language server is lsp_server.gpr
source/
   ada/      # Implementation for Ada server requests
   gpr/      # Implementation for GPR server requests
   lsp_gen/  # A generated Ada binding to the Language Server Protocol (LSP) - generated from the JSON schema
   server/   # The server infrastructure
   client/   # LSP client glu code
testsuite/   # The testsuite
   ada_lsp/  # The tests for the Ada language server
```

## Building and testing

Build with `make`.

Run tests with `./testsuite/testsuite.py`.
Run an individual test or a series of tests with `./testsuite/testsuite.py <test_name_or_substring>`.

## Ada formatting

[LSP_Common.Format](gnat/lsp_common.gpr) shares the formatting settings between
the Ada projects. [gnatformat.json](gnat/gnatformat.json) overrides selected
nodes in GNATformat's default layout:

- Keep `overriding` and `not overriding` beside `procedure` or `function`.
- Put multiple parameter specifications on separate lines and align their
  colons. Parameter types are not padded to align after `in` and `out`.
- Put named aggregate associations on separate lines and align `=>`.
  Positional aggregates keep the default layout.
- Split calls with multiple associations when named notation is used. Keep
  the generic name beside `new` when it fits.

The line width is 79, indentation is three spaces, continuation indentation is
two spaces, and source encoding is UTF-8. Comment wrapping and `is` placement
keep GNATformat's default behavior. The parameter layout is an approximation
of the former GNATpp settings, not an exact copy of `--par-threshold=2`.

The association and instantiation layouts come from Libadalang's GNATpp
compatibility snippets for `--call-threshold=1` and
`--vertical-named-aggregates`, with the latter restricted to named associations.
They were checked with GNATformat 28.0w (20261116). A node override replaces its
configuration, so keep its field templates as well as its node template.
Review preview diffs when updating GNATformat or Libadalang.

From the repository root, preview one file without changing it:

```sh
gnatformat -P gnat/formatting.gpr --pipe source/ada/lsp-ada_handlers.adb
```

To format selected files, pass their names explicitly:

```sh
gnatformat -P gnat/formatting.gpr source/ada/lsp-ada_handlers.adb
```

Use this separate project for command-line formatting: the server project uses
global preprocessing symbols that GNATformat cannot load. The editor uses the
shared settings through the loaded Ada project.

The GNATformat pre-commit hook runs automatically on changed Ada files, including
in CI. It formats each selected file in full so that files adopt the shared
style as they are changed. Review the formatting changes along with the rest of
the file's changes. To run the hook explicitly:

```sh
pre-commit run gnatformat --files source/ada/lsp-ada_handlers.adb
```

Avoid formatting the entire project or regenerating protocol sources as part
of a small functional change. Test fixtures and dependency checkouts remain
excluded from the hook.

## Server architecture

The server consists in

- a server task containing a main loop that listens for incoming requests
- a processing task taking care of handling the requests

The processing task is the one containing and maintaining a Libadalang context
which represents the project being loaded.

## Checklist for contributions

* Run `make` and `./testsuite/testsuite.py`.
* Add a test for your change if applicable.
* Update `dev_guide.md` as needed.
