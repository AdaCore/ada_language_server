Tester
======

> Command line tool to test Language Server Protocol server.

Tester accepts a test scenario described in JSON file.

Test running
------------

Before you start the tester put into the `ALS` environment
variable the language server command line:

```
export ALS=.objs/server/ada_language_server
```

To start a test provide JSON file as argument to tester-run:
```
tester-run test.json
```

If you want to debug the server with the `GDB`, then use
`--debug` option:
```
$ tester-run --debug test.json
Language server is running. You can attach it with GDB.
Press ENTER to continue.
```

In this case the `tester-run` pauses the execution after
launching the server and let you attach `gdb` to it.
Just find PID of the server and invoke GDB:
```
gdb --pid=<PID> .obj/server/ada_language_server
```

Press `ENTER` to signal the tester to continue.


JSON file format
----------------

Test scenario is JSON array of _command_ object. Such object has
just one property. Command kind is taken from property name.
Propetry value depends on command kind.

Here is list of supported commands.

### Command `start`

Property value - an object:
 * "cmd" - array of string.
 * "waitFactor" - see "Execution timeouts"

Start new LSP server using _cmd_ as command line.

### Command `stop`

Property value - an object:
 * "exit_code" - expected exit code of LSP process.
 * "close_stdin" - optional boolean, default true, if tester will close stdin
   before waiting for server termination.
 * "waitFactor" - see "Execution timeouts"

Close LSP server pipe and wait until server stop.

### Command `send`
Property value - an object:

 * "request" - JSON object to send to LSP server as request.
 * "wait" - array of _wait_ objects to expect them in any order.
 * "sortReply" - an object describing how to sort a server reply. Let's
   explain by examples:
    1.  `"sortReply": {"result": "uri"}` - Server reply should have a property
       `result`, that is an array of objects, each of them has a property
       `uri`. Tester driver will sort the array using the `uri` as a sort key.
    2. `"sortReply": { "result": ["label", "detail"] }` - you can have a
       composite sort key, if you provide names of properties as an array
       of strings.
    3. `"sortReply": { "result": { "items": ["label", "detail"] } }` - if the
       server reply has a JSON array wrapped in an object, you can nest a sort
       desriptor into an object. In this case in the server reply `result` is
       an object, that has a `items` property. Where `items` is an array of
       objects, that should be sorted using the `label` and `detail` as a
       composite sort key.
    4. `"sortReply": { "result": { "from": "uri" } }` - Server reply should
       have a property `result`, that is an array of objects. Array items
       have property `from` which is an object. Tester driver will sort the
       array using the `uri` property of `from` objects as a sort key.
 * "waitFactor" - see "Execution timeouts"

Where _wait_ object is expected server answer. Each property of this object
should be in server response, but some values have a special meaning:
 * string `<ANY>`  - matches any string value
 * string `<ABSENT>` - ensures that there is no such property at all
 * array `['<HAS>', item1, item2, ...]` - ensures that all given items are
   included into the array, any other array items are considered irrelevant and ignored
 * array `['<DOES_NOT_HAVE>', item1, item2, ...]` - ensures that all given items are
   not included into the array, any other array items are considered irrelevant and ignored

### Command `shell`

Property value - array of strings.

Tester launches an OS process taking command and arguments from the array.
The primary purpose is to launch a Python like this:

    "shell": ["${PYTHON}", "${DIR}/makelink.py" ]

### Command `comment`

Property value - array of strings or just string.

Tester just ignores this command. We use it to add test desription and other
comments to JSON test script.

Execution timeouts
------------------

Each command has a limited time to run. The current timeout is 5 seconds.
The `send` command has 4 seconds to complete, but each server message resets
the timer, so while server sends messages the command keeps running.
This helps for runtime library indexing when progress report messages come
with regular interval, but whole indexing could be long.

The timeout can be increased several times by setting by setting
`ALS_WAIT_FACTOR` environment variable. A particular command can increase
its timeout with `waitFactor` property.

JSON file preprocessing
-----------------------

Before execution Tester does some text substitution in each string literal.
 * Each substring `${NAME}` is replaced by an environment variable with
given NAME. The `DIR` environment variable points to test's directory.

 * Each substring `$URI{x}` is replaced by corresponding URI `file:///test_dir/x`.
where `x` should be path relative to the directory where `.json` file is located.

