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
 "cmd" - array of string.

Start new LSP server using _cmd_ as command line.

### Command `stop`

Property value - an object:
 "exit_code" - expected exit code of LSP process.

Close LSP server pipe and wait until server stop.

### Command `send`
Property value - an object:

 * "request" - JSON object to send to LSP server as request.
 * "wait" - array of _wait_ objects to expect them in any order.

 Where _wait_ object is one of
  * "response" - expected server ansver. Each propert of this object should
                 be in server response
  * "notification" - TBD
