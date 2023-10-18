How to Extend the Protocol for Ada
==================================


The Ada Language Server (ALS) supports some features that are not in the official
[Language Server Protocol](https://microsoft.github.io/language-server-protocol)
specification. This document specifies how these features are documented.

## Developing a custom feature

Usually we tend to implement custom features through [LSP commands](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#command). These commands can then be executed by the LSP clients running the ALS (e.g: VS Code).

Commands can either do something on the codebase directly (e.g: refactorings) or be used to query specific information (e.g: return all the function declarations within a scope).

Commands can be directly accessible to users through [LSP codeActions](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_codeAction). This is the case for all ALS-specific refactorings.

You can also use custom commands to perform queries on the code base, to develop an IDD-specific tool integration on top of the ALS for instance. In that case you can directly execute the command via the [LSP workspace/executeCommand](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_executeCommand) request and use the command's results as you want.

Here are some implementations of custom features in the ALS implemented through this mechanism:

* [Show Dependencies](../source/ada/lsp-ada_handlers-show_dependencies_commands.ads)
* [Other File](../source/ada/lsp-ada_handlers-other_file_commands.ads)

## Feature description document

Each feature is described in a dedicated Markdown document.
There is [a list of all features](.#list-of-features) at the end of this document. 
We provide a [template](./TEMPLATE.md) for convenience.
The feature description document has the following structure:

### Title (feature name)
The title matches the feature description document file name.

### Short introduction
A short introduction describes purpose of the extension.

### Capabilities [optional]
If a given feature requires some incompatible changes to the LSP protocol,
then the client should request the explicit Capability to make the server
send the extended information. This is required only in places where we
replace some standard JSON properties. We can add new JSON properties
without breaking compatibility, because clients simply ignore unknown properties.

This section provides a description of additional capabilities of the
`Initialize` request if required.

We use the `als` prefix in additional properties, to avoid name collisions.

### Change description
This section describes what requests, responses and notifications are changed.
It includes TypeScript definitions, as the official specification does.

### See also [optional]
This section includes related feature description documents and links to others
related resources.

## List of custom features
 * [Debug](debug.md)
 * [Other File](other_file.md)
 * [Reference kinds](reference_kinds.md)
 * [Show Dependencies](show_dependencies.md)
 * [Check Syntax](check_syntax.md)
 * [Executables](executables.md)