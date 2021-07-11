How to Extend the Protocol for Ada
==================================


The Ada Language Server (ALS) supports some features that are not in the official
[Language Server Protocol](https://microsoft.github.io/language-server-protocol)
specification. This document specifies how these features are documented.

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

## List of features
 * [Debug](debug.md)
 * [Other File](other_file.md)
 * [Reference kinds](reference_kinds.md)
 * [Show Dependencies](show_dependencies.md)
 * [Check Syntax](check_syntax.md)
