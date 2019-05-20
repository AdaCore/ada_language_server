Testsuite
=========

This project has several testsuite tools

== Tester ==
[Tester](ada_lsp/README.md) emulates LSP client.
It reads commands from JSON file and executes them. See mo details in
[Tester](ada_lsp/README.md).

== Codec_Test ==
Codec_Test check errors in encoders/decoders of LSP messages.
It reads LSP message from JSON file, converts it to an Ada object, then
it converts the Ada object back to JSON and compares result with origin
JSON object. Codec_Test has no command line arguments. Instead it reads
lines standard input. Each like has file name and Ada type name separated
by space. Empty lines and lines started with '#' are ignored.
