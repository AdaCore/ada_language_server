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

## Server architecture

The server consists in

- a server task containing a main loop that listens for incoming requests
- a processingtask taking care of handling the requests

The processing task is the one containing and maintaining a Libadalang context
which represents the project being loaded.

## Checklist for contributions

* Run `make` and `./testsuite/testsuite.py`.
* Add a test for your change if applicable.
* Update `dev_guide.md` as needed.
