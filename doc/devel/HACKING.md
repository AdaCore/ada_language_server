# Developing on the Ada Language Server

## Dependencies

The Ada Language Server depends on a number of tools and libraries listed [on the main page](https://github.com/AdaCore/ada_language_server#dependencies).

## Coding

The ALS repository includes a template VS Code workspace at
`.vscode/settings.json.tmpl`. This allows developers to instantiate the
workspace locally and customize it to their liking, in particular the
`terminal.integrated.env.<os>` setting to set `PATH` and `GPR_PROJECT_PATH` as
needed to provided tools and dependencies to the development environment.

To instantiate the VS Code workspace, simply run `make configure`. Then open VS
Code at the root of the ALS repository.

At the first run, the workspace will recommend installing a set of extensions
including a released version of the `Language Support for Ada` extension which
is produced from this repository. That allows you to work with the Ada codebase
of the ALS.

## Building

See [build.md](build.md) for details.

## Debugging

One way to run the ALS is by running the VS Code extension in development mode.
You can do that using the VS Code workspace at the root of this repository and the launch configuration `(vscode) Launch vscode with the Ada extension`.

You can activate traces that show all the server input/output. This is done
by creating a file `$HOME/.als/traces.cfg` with the following contents:

```
ALS.IN=yes > inout.txt:buffer_size=0
ALS.OUT=yes > inout.txt:buffer_size=0
```

When this is present, the Ada Language Server will generate a file
`$HOME/.als/inout.txt` which logs the input received and the output sent by
the language server.

You can also monitor the server's memory usage by adding the following lines:

```
>ada_ls_log.$T.txt:buffer_size=0:buffer_size=0
ALS.MAIN=yes
DEBUG.ADA_MEMORY=yes
```

This will create `ada_ls_log.<timestamp>.txt` log files in your `$HOME/.als`
directory each time the Ada Language Server is run, with additional information
about memory usage, allowing to track down which parts of the program consumes
the most memory.
This is very useful to resolve unexpected memory consumption issues.

## Getting symbolic backtrace

To get a symbolic backtrace you need the debug information files. For the
release you can download them from
[GitHub Release](https://github.com/AdaCore/ada_language_server/releases)
Assets. Assets contain an archive per platform with the ALS
executable and debug information files (`.debug` file on Linux/Windows and
`.dSYM` directory for Mac OS X). Extract the debug information to the directory
containing ALS (usually this is
`$HOME/.vscode/extensions/adacore.ada-*/{darwin,linux,win32}`)
and run

    addr2line ada_language_server <hex backtrace>

On Mac OX X use [atos](https://www.unix.com/man-page/osx/1/atos/) instead.

## Getting symbolic backtrace for TypeScript

The Ada & SPARK extension for Visual Studio Code is published as minified
JavaScript code. For that reason backtraces reported are not easily readable
by humans. To alleviate that, the published .vsix includes a source map file
at `out/src/extension.js.map`.

To resolve a backtrace to the original TypeScript source locations:

1. Unzip the .vsix

2. Copy the backtrace into a plain text file *while preserving the error message
on the first line*, for example:

    ```
    2024-01-11 10:11:54.191 [Ada Extension] ERROR    Error while starting Ada extension...
        at Gu (/home/XXXXXXX/.vscode/extensions/adacore.ada-24.0.3/out/src/extension.js:75:5911)
        at oS (/home/XXXXXXX/.vscode/extensions/adacore.ada-24.0.3/out/src/extension.js:75:6057)
        at w$ (/home/XXXXXXX/.vscode/extensions/adacore.ada-24.0.3/out/src/extension.js:85:2292)
        at b$ (/home/XXXXXXX/.vscode/extensions/adacore.ada-24.0.3/out/src/extension.js:85:2154)
        ...
    ```

3. Run the following command to obtain source locations:

   ```
   $ npx stacktracify out/src/extension.js.map -f trace.txt
   2024-01-11 10:11:54.191 [Ada Extension] ERROR    Error while starting Ada extension...
       at [unknown] (../../src/helpers.ts:150:56)
       at getEvaluatedCustomEnv (../../src/helpers.ts:186:22)
       at assertSupportedEnvironments (../../src/extension.ts:128:4)
       at activateExtension (../../src/extension.ts:118:14)
   ```

### Writing tests

To write a functional test for Ada Language Server:

  * Choose a meaninful name for your test, for instance `completion_inside_generics`.
     We'll refer to this as `<testname>` below
  * Create a new directory `testsuite/ada_lsp/<testname>` containing your test data
  * Activate full in/out language server traces. See the Debugging section above.
  * From the base directory in this repository, run:
     ```
     python scripts/traces_to_test.py <testname> <path_to_the_als_traces_file>
     ```
  * Delete all extra requests, notifications and capabilities that are not related
    to the feature being tested. It will help a lot later when tests are
    baselined because of changes in protocol, capablitities or message formats.
  * Replace the comment at the beginning of the test with something meaningful:
  ```
      "comment": [
         "test automatically generated"
      ]
  ```

### Running tests

 * run `make check` to run the entire testsuite
 * to run an individual test, go to `testsuite` and run `sh run.sh ada_lsp/<testname>`
    (you will need `https://github.com/AdaCore/e3-testsuite` installed to do this)

### VS Code integration tests

Run `make vscode-test` to run the VS Code testsuite.

If you open the ALS repository in VS Code, it is also possible to run VS Code
integration tests using the Testing view.
The `integration/vscode/ada/.vscode-test.mjs` contains a configuration allowing to load the tests in the Testing view.
The UI offers ways to run or debug one test, a subset of tests or all tests.

### Other tests

See more about the project testsuite [here](https://github.com/AdaCore/ada_language_server/blob/master/testsuite/README.md).

## Release VS Code extension

To release a new version of the VS Code extension, just put a tag with of the form
`22.0.3`, where `22.0` matches the current GNAT Studio development version. The
corresponding `git` command is
```
   git checkout master
   git tag -a 22.0.3
```

Put release notes in the comment of the tag starting with a `Release notes` header:
```
Release notes

* <notes 1>
* <notes 2>, etc.
```

Then push it to GitHub with `git push 22.0.3` or `git push --tags`.
The GitHub CI will publish the extension on the marketplace and the
[Open VSX Registry](https://open-vsx.org). It also makes GitHub Release and
put archives with ALS executables and debug information files.

## Source directories

The repository contains the following folders:

* `doc/` - documentation of the project and LSP extensions
* `gnat/` - GNAT project files
* `integration/vscode/ada/` - VS Code extension sources
* `scripts/` - some support scripts
* `ada_language_server/source/ada/` - LSP implementation for Ada language
* `source/client/` - basic client interface
* `source/protocol/` - LSP binding in Ada
* `source/server/` - Common LSP server implementation
* `source/spawn/` - Process spawn/communication API
* `source/tester/` - source of the test driver
* `testsuite/ada_lsp/` - test suite of LSP in form of request/response
* `subprojects/` - where we store dependencies (VCS, ada_libfswatch)
* `subprojects/stubs` - the location for "stub" versions of .gpr files, for
                        dependencies that are optional.

## Protocol synchronization

You can get the latest Language Server Protocol specification from
the https://github.com/microsoft/language-server-protocol.git repository,
`gh-pages` branch.

Currently, we synchronize Ada sources with LSP updates manually. The Ada source to
synchronize is the `LSP.Messages` package. The specification of the package contains
TypeScript snippets from the Language Server Protocol specification in the form of
comments:
```
   --```typescript
   --interface Message {
   --   jsonrpc: string;
   --}
   --```
```

You can easily extract all such snippets from the specification with the command
```
sed -n -e '/^```typescript/,/^```/p' specification-3-14.md |sed -e 's/^/   --/'
```

To extract corresponding snippets from `lsp-messages.ads` use this:
```
sed -n -e '/^   --```typescript/,/^   --```/p' lsp-messages.ads
```

Unfortunately we have to reorder Ada declarations to follow _"define before use"_
rule. To restore original order use `scripts/reorder.py` script. Then you can
compare texts with `diff -u` to see what's changed.

Rules to match method name with corresponding messages are not present in the
TypeScript snippets. So we don't put requests, responses and notifications types
in then `LSP.Messages` package. Instead we have 6 packages divided by message
kind and directions:

* LSP.Messages.Server_Requests - requests from the client to the server
* LSP.Messages.Server_Notifications - notifications from the client to the server
* LSP.Messages.Server_Responses - responses from the server to the client
* LSP.Messages.Client_Requests - requests from the server to the client
* LSP.Messages.Client_Notifications - notifications from the server to the client
* LSP.Messages.Client_Responses - responses from the client to the server

`Server_Requests` and `Server_Notifications` packages are generated by the
`scripts/generate.py` script.

Each of these 6 packages defines common root type and a `Visit` subprogram
to implement Visitor pattern. Corresponding visitors are defined in
packages:

* LSP.Server_Request_Receivers
* LSP.Server_Notification_Receivers
* Server_Response_Sender
* LSP.Client_Request_Receivers
* LSP.Client_Notification_Receivers
* LSP.Client_Response_Senders

The type `Message_Logger` implements all of these to provide custom format
for each message kind in the trace.

The LSP server uses its-own `Server_Request_Handler` interface. This
interface defines functions corresponding to client-to-server LSP requests.
Function profiles ensure correct argument and result types at compile time.

`LSP.Ada_Handlers` implements `Server_Request_Handler` and
`Server_Notification_Receiver` interfaces to provide Ada language support.
It leverages `Server` to send notification to the client.

`Error_Decorator` also implements `Server_Request_Handler` and
provides safety net to catch Property_Error exceptions during
request processing.

LSP `Server`, on its side, converts messages to/from JSON,
tracks request Ids and implements the `CancelRequest` logic.

The `LSP.Ada_Documents.Document` type represents a file opened in an editor.
It keeps text of a changed file.

The `LSP.Ada_Contexts.Context` type represents non-aggregate project tree
and keeps a Libadalang context to process compilation units from this tree.

`Document` and `Context` objects belong to `Ada_Handler`.
