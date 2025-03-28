# Ada Language Server configuration

## Configuration Sources

The ALS loads configuration settings from different sources. All configuration sources are loaded in the following order:

1. A global user configuration file `$XDG_CONFIG_HOME/als/config.json`, if it exists. The path [defaults](https://specifications.freedesktop.org/basedir-spec/0.8/#variables) to `$HOME/.config/als/config.json` if `XDG_CONFIG_HOME` is not set.

1. A workspace-specific `.als.json` file in the directory where ALS is spawned, if it exists.

   This is the prefered location to store project-specific settings that are tracked in version control and shared among developers.

1. The `--config CONFIG_FILE` file, if specified in the command line.

1. The `initializationOptions` property of the `initialize` request, if specified.

1. In `workspace/didChangeConfiguration` LSP notifications, if specified.

Each configuration source can contain a partial list of settings. Thus each
configuration source can override individual settings while preserving
previously loaded settings.

Configuration files must be JSON files matching [this JSON schema](../integration/vscode/ada/schemas/als-settings-schema.json). Roughly the structure looks like this:

```json
{
    "projectFile": "...",
    "scenarioVariables": { ... },
    ...
}
```

If specified in the `initialize` request, settings should be wrapped into an `ada` JSON object in the `initializationOptions` property as follows:

```json
{
    "jsonrpc": "2.0",
    "method": "initialize",
    "params": {
        "initializationOptions": {
            "ada": {
                "projectFile": "right_project.gpr",
                "scenarioVariables": { ... },
                ...
            }
        },
        ...
    }
}
```

Similarly, settings passed in `workspace/didChangeConfiguration` notifications should also be wrapped into an `ada` JSON object:

```json
{
    "jsonrpc": "2.0",
        "method": "workspace/didChangeConfiguration",
        "params": {
        "settings": {
            "ada": {
                "projectFile": "right_project.gpr"
            }
        }
    }
}
```

## Base Configuration

The *base configuration* is the one that ALS reaches after loading configuration files (i.e. global user configuration, workspace-specific `.als.json` file, and `--config` command line argument).

After that the ALS may receive configuration changes through the `initialize` request, or the `workspace/didChangeConfiguration` notification. In those messages, if settings have the value `null`, ALS reverts their value to the base configuration. This allows clients to temporarily override settings, and revert them back in the same session.

## Visual Studio Code

In the context of Visual Studio Code, configuration settings can be set in the
User, Remote or Workspace `settings.json` file or the [multi-root workspace
file](https://code.visualstudio.com/docs/editor/multi-root-workspaces) by
prefixing each setting name with `ada.`, e.g.

```json
{
    "ada.projectFile": "right_project.gpr",
    "ada.scenarioVariables": {
        "LIBRARY_TYPE": "static"
    },
    "ada.onTypeFormatting.indentOnly": true,
    "ada.useGnatformat": true
}
```

These settings are sent to the ALS in the LSP `initialize` request, and then in `workspace/didChangeConfiguration` notifications if they get updated.

## Settings

Settings taken into account only from the Ada & SPARK VS Code extension:

* [showNotificationsOnErrors](#shownotificationsonerrors)
* [trace.server](#traceserver)

Settings understood by the Ada Language Server itself, independently from the LSP client:

* [projectFile](#projectfile)
* [gprConfigurationFile](#gprconfigurationfile)
* [scenarioVariables](#scenariovariables)
* [defaultCharset](#defaultcharset)
* [relocateBuildTree](#relocatebuildtree)
* [rootDir](#rootdir)
* [enableDiagnostics](#enableddiagnostics)
* [adaFileDiagnostics](#adafilediagnostics)
* [gprFileDiagnostics](#gprfilediagnostics)
* [sourceInfoDiagnostics](#sourceinfodiagnostics)
* [projectDiagnostics](#projectdiagnostics)
* [alireDiagnostics](#alirediagnostics)
* [enableIndexing](#enableindexing)
* [renameInComments](#renameincomments)
* [namedNotationThreshold](#namednotationthreshold)
* [foldComments](#foldcomments)
* [useCompletionSnippets](#usecompletionsnippets)
* [insertWithClauses](#insertwithclauses)
* [followSymlinks](#followsymlinks)
* [documentationStyle](#documentationstyle)
* [onTypeFormatting.indentOnly](#ontypeformattingindentonly)
* [useGnatformat](#usegnatformat)

----

### showNotificationsOnErrors

Controls whether VS Code should display error notifications for failing LSP requests.

This is disabled by default to avoid distractions while coding, and can be enabled if the User wishes to troubleshoot an unexpected behavior in the Ada & SPARK language support in VS Code.
Note that regardless of this setting, errors are always logged to the VS Code output channels `Ada Language Server` and `GPR Language Server`. This setting only controls whether a corresponding visual notification is emitted in the UI.

### trace.server

This option controls the tracing of the communication between VS Code and the Ada
language server. It causes the client to trace each message sent and received
to/from the Ada language server in the `Ada Language Server` Output view.

The possible values are:

* `off`: no tracing.
* `messages`: brief traces are emitted for each request sent and each response received.
* `verbose`: verbose traces are emitted for each request sent and each response received, including the message content.

On the server side this option does not trigger any additional logging.

An equivalent setting `gpr.trace.server` exists for tracing the communcation between VS Code and the GPR language server.

### projectFile

You can configure the GNAT Project File via the `projectFile` key.
The setting has a string value, that points to the `.gpr` file.
It could be a full path or relative path.
It could be prefixed with `file:` schema.
If no project provided and there is a single project file in the
root folder, then ALS will use it.

```javascript
    'projectFile': 'gnat/lsp_server.gpr'
```

### gprConfigurationFile

You can configure the GPRBuild configuration file via the
`gprConfigurationFile` key. The setting has a string value, that points to
the `.cgpr` file. It could be a full path or relative path.

```javascript
    'gprConfigurationFile': 'gnat/lsp_server.cgpr'
```

### scenarioVariables

You can configure scenario variables via the `scenarioVariables` key.
The setting has an object value. Keys in this object correspond to
scenario variables names and string values to variables values.

```javascript
    'scenarioVariables': {
        'BUILD_MODE': 'DEBUG'
    }
```

### defaultCharset

You can set the character set to use when the server has to use when reading
files from disk by specifying an `defaultCharset` key. The default is
`iso-8859-1`. This should have a string value.

```javascript
    'defaultCharset': 'UTF-8'
```

### relocateBuildTree

With this option it is possible to achieve out-of-tree build. That is,
real object, library or exec directories are relocated to the current
working directory or dir if specified. Ensure that it is full normalized
path ended with the directory separator. Visit
the [gprbuild documentation](https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/building_with_gprbuild.html#switches)
for more details about the corresponding gprbuild switch.

```javascript
    'relocateBuildTree': '/home/user/project/build/'
```

### rootDir

This option is to be used with relocateBuildTree above and cannot be
specified alone. This option specifies the root directory for artifacts
for proper relocation. Ensure that it is full normalized path ended
with the directory separator. Visit
the [gprbuild documentation](https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/building_with_gprbuild.html#switches)
for more details about the corresponding gprbuild switch.

```javascript
    'relocateBuildTree': '/home/user/project/'
```

### enableDiagnostics

This setting has been deprecated please have a look at `projectDiagnostics`,
`adaFileDiagnostics` and `alireDiagnostics`.

```javascript
    'enableDiagnostics': false
```

### adaFileDiagnostics

You can explicitly deactivate the emission of diagnostics for Ada Files
via the `adaFileDiagnostics` key. By default, diagnostics are enabled.
The value is a boolean.

```javascript
    'adaFileDiagnostics': false
```

### gprFileDiagnostics

You can explicitly deactivate the emission of diagnostics related to the
edition of gpr Files via the `gprFileDiagnostics` key. By default,
diagnostics are enabled.
The value is a boolean.

```javascript
    'gprFileDiagnostics': false
```

### sourceInfoDiagnostics

You can explicitly deactivate the emission of source infomration diagnostics
when opening Ada files via the `sourceInfoDiagnostics` key. By default,
diagnostics are enabled.
The value is a boolean.

```javascript
    'sourceInfoDiagnostics': false
```

### projectDiagnostics

You can explicitly deactivate the emission of diagnostics when loading a
project via the `projectDiagnostics` key. By default, diagnostics are enabled.
The value is a boolean.

```javascript
    'projectDiagnostics': false
```

### alireDiagnostics

You can explicitly deactivate the emission of diagnostics when loading an
alire crate via the `alireDiagnostics` key.
By default, diagnostics are enabled.
The value is a boolean.

```javascript
    'alireDiagnostics': false
```

### enableIndexing

By default, the server indexes the source files after loading a project
to speed up subsequent requests. This behavior can be controlled
via the `enableIndexing` flag in this request.
The value is a boolean.

```javascript
    'enableIndexing': false
```

### renameInComments

The language server is able to edit Ada comments while executing
`textDocument/rename` request. To enable this just set
`renameInComments` setting to `true`.
The value is a boolean.

```javascript
    'renameInComments': false
```

### useCompletionSnippets

Whether we should use snippets in completion results. Snippets can be
returned in case of subprogram calls for instance, with placeholders
for each parameter needed by the subprogram.
The value is a boolean.

```javascript
    'useCompletionSnippets': true
```

### insertWithClauses

Whether we should automatically insert missing with-clauses when
accepting completion for invisible symbols.

```javascript
    'insertWithClauses': true
```

### displayMethodAncestryOnNavigation

This setting controls the policy for displaying overriding and overridden
subprograms on navigation requests such as `textDocument/definition`,
`textDocument/declaration` or `textDocument/implementation`.

The different policies are:

* `never`: Never list overridding and/or overridden suprograms.
* `usage_and_abstract_only`: List overridding and/or overridden suprograms
   on dispatching calls and on abstract subprogram declarations.
* `definition_only`: List overridding and/or overridden suprograms on
   declarations only.
* `always`: Always list overridding and/or overridden suprograms when
   possible.

```javascript
    'displayMethodAncestryOnNavigation': 'always'
```

### namedNotationThreshold

This setting defines the number of parameters/components at which point named
notation is used for subprogram/aggregate completion snippets.
The value is a number. The default value is `3`.

```javascript
    'namedNotationThreshold': 2
```

### foldComments

When this setting is `true` the server sends blocks information for comments which can be used for folding comment blocks.
The value is a boolean. The default is `true`.

```javascript
    'foldComments': false
```

### followSymlinks

When this setting is `false` the server doesn't do any attempts to normalize file names sent by a client.
This means that symlinks stay unresolved and character case is unchanged (on case insensitive file systems).
This setting mainly is for the GNAT Studio integration.
The value is a boolean. The default is `true`.

```javascript
    'followSymlinks': false
```

### documentationStyle

The language server supports different styles to document entities in the source
code. This setting controls primary documentation style of entities. When
documentation for the entity is not found, the language server uses a few
mechanisms to find the best fallback (lookup for comments before/after entity
declaration, extract documentation from subprogram's body, etc.)

Supported styles are:

* `gnat`: Default style, based on GNAT coding standard with some
  enhancements.
* `leading`: Documentation for the entities extracted from the comments
  before the entity declaration.

For more information about documentation styles see GNATdoc User's Manual.

```javascript
    'documentationStyle': 'gnat'
```

### onTypeFormatting.indentOnly

This option controls if the `textDocument/onTypeFormatting` request only indents a new line, or if
it additionally tries to format the previous node. By default, this option is enabled, that is,
`textDocument/onTypeFormatting` only indents new lines.

In ALS config files, this setting must be specified in a nested form:

```json
{
   "onTypeFormatting": {
      "indentOnly": true
   }
}
```

Conversely, in VS Code this settings can be set without nesting:

```json
{
   "ada.onTypeFormatting.indentOnly": true,
}
```

### useGnatformat

This option controls the formatting provider for the `textDocument/formatting`,
`textDocument/rangeFormatting` and `textDocument/onTypeFormatting` request. By default, this option
is enabled and ALS uses GNATformat as its formatting provider. If disabled, GNATpp is used instead.

### logThreshold

Controls the maximum number of trace files preserved in the ALS log directory (which defaults to `~/.als`).
When this threshold is reached, old trace files get deleted automatically.
The default number of preserved trace files is `10`.
See the documentation on [ALS Traces](traces.md) for more information.
