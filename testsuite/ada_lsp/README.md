Tester
======

> Command line tool to test Language Server Protocol server.

Tester accepts a test scenario described in JSON file.

Test running
------------

To start a test provide JSON file as argument to tester-run:
```
tester-run test.json
```

JSON file format
----------------

Test scenario is JSON array of _command_ object. Such object has
just one property. Command kind is taken from property name.
Propetry value depends on command kind.

Here is list of supported commands.

### Command `start`

Property value - an object:
 * "cmd" - array of string.

Start new LSP server using _cmd_ as command line.

### Command `stop`

Property value - an object:
 * "exit_code" - expected exit code of LSP process.
 * "close_stdin" - optional boolean, default true, if tester will close stdin
before waiting for server termination.

Close LSP server pipe and wait until server stop.

### Command `send`
Property value - an object:

 * "request" - JSON object to send to LSP server as request.
 * "wait" - array of _wait_ objects to expect them in any order.
 * "sortReply" - an object with properies. Each property has string value of
a _key_. The property name points to a property in the server reply to
be sorted; it should be JSON array of object. While _key_ is property name
in the array item to compare.

Where _wait_ object is expected server answer. Each property of this object
should be in server response, but some string values have a special meaning:
 * `<ANY>`  - matches any string value
 * `<ABSENT>` - ensures than there is no such property at all

### Command `comment`

Property value - array of string or just string.

Tester just ignores this command. We use it to add test desription and other
comments to JSON test script.



JSON file preprocessing
-----------------------

Before execution Tester does some text substitution in each string literal.
 * Each substring `${NAME}` is replaced by an environment variable with
given NAME.

 * Each substring `$URI{x}` is replaced by corresponding URI `file:///test_dir/x`.
where `x` should be path relative to the directory where `.json` file is located.

