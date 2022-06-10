# Ada Language Server configuration

The LSP has a dedicated notification to signal the server the change of configuration settings. Any setting should be inside a `ada` JSON object.
Ada Language Server understands these settings:

 * [projectFile](#projectFile)
 * [scenarioVariables](#scenarioVariables)
 * [defaultCharset](#defaultCharset)
 * [relocateBuildTree](#relocateBuildTree)
 * [rootDir](#rootDir)
 * [enableDiagnostics](#enableDiagnostics)
 * [enableIndexing](#enableIndexing)
 * [renameInComments](#renameInComments)
 * [namedNotationThreshold](#namedNotationThreshold)
 * [foldComments](#foldComments)
 * [followSymlinks](#followSymlinks)
 * [documentationStyle](#documentationStyle)

----



## projectFile

You can configure the GNAT Project File via the `projectFile` key.
The setting has a string value, that points to the `.gpr` file.
It could be a full path or relative path.
It could be prefixed with `file:` schema.
If no project provided and there is a single project file in the
root folder, then ALS will use it.

```javascript
    'projectFile': 'gnat/lsp_server.gpr'
```

## scenarioVariables
You can configure scenario variables via the `scenarioVariables` key.
The setting has an object value. Keys in this object correspond to
scenario variables names and string values to variables values.

```javascript
    'scenarioVariables': {
        'BUILD_MODE': 'DEBUG'
    }
```
## defaultCharset
You can set the character set to use when the server has to use when reading
files from disk by specifying an `defaultCharset` key. The default is
`iso-8859-1`. This should have a string value.

```javascript
    'defaultCharset': 'UTF-8'
```

## relocateBuildTree
With this option it is possible to achieve out-of-tree build. That is,
real object, library or exec directories are relocated to the current
working directory or dir if specified. Ensure that it is full normalized
path ended with the directory separator. Visit
https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/building_with_gprbuild.html#switches
for more details about the corresponding gprbuild switch.
```javascript
    'relocateBuildTree': '/home/user/project/build/'
```

## rootDir
This option is to be used with relocateBuildTree above and cannot be
specified alone. This option specifies the root directory for artifacts
for proper relocation. Ensure that it is full normalized path ended
with the directory separator. Visit
https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/building_with_gprbuild.html#switches
for more details about the corresponding gprbuild switch.
```javascript
    'relocateBuildTree': '/home/user/project/'
```

## enableDiagnostics
You can explicitly deactivate the emission of diagnostics, via the
`enableDiagnostics` key. By default, diagnostics are enabled.
The value is a boolean.

```javascript
    'enableDiagnostics': false
```

## enableIndexing
By default, the server indexes the source files after loading a project,
to speed up subsequent requests. This behavior can be controlled
via the `enableIndexing` flag in this request.
The value is a boolean.

```javascript
    'enableIndexing': false
```

## renameInComments
The language server is able to edit Ada comments while executing
`textDocument/rename` request. To enable this just set
`renameInComments` setting to `true`.
The value is a boolean.

```javascript
    'renameInComments': false
```

## useCompletionSnippets
Whether we should use snippets in completion results. Snippets can be
returned in case of subprogram calls for instance, with placeholders
for each parameter needed by the subprogram.
The value is a boolean.

```javascript
    'useCompletionSnippets': true
```

## displayMethodAncestryOnNavigation
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

## namedNotationThreshold
This setting defines the number of parameters/components at which point named
notation is used for subprogram/aggregate completion snippets.
The value is a number. The default value is `3`.

```javascript
    'namedNotationThreshold': 2
```

## foldComments
When this setting is `true` the server sends blocks information for comments which can be used for folding comment blocks.
The value is a boolean. The default is `true`.

```javascript
    'foldComments': false
```
## followSymlinks

When this setting is `false` the server doesn't do any attempts to normalize file names sent by a client.
This means that symlinks stay unresolved and character case is unchanged (on case insensitive file systems).
This setting mainly is for the GNAT Studio integration.
The value is a boolean. The default is `true`.

```javascript
    'followSymlinks': false
```

## documentationStyle
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
