# Contributing

This document will describe the necessary information required to contribute to the [VS Code](https://code.visualstudio.com/) extension.

## Developer Setup

It is recommended to use the VS Code workspace defined at the root of this Git
repository for development of the VS Code Ada extension. Run this initially to install all dependencies:

```
cd integration/vscode/ada; npm install
```

And then use the task `npm: compile` for compilation and the launch
configuration `Launch Extension` for local testing and debugging.

### macOS on Apple M1/M2 processors

On macOS with Apple M1/M2 processors, using a native arm64 `node`/`npm` will
result in an error as the VS Code Ada extension is not yet ready to support the
arm64 architecture on all platforms. To work around this, install and use an
x86_64 version of Node.js. You can download it from the
[Node.js](https://nodejs.org/en/download) website or using an x86_64
installation of [Homebrew](https://brew.sh/).

Homebrew x86_64 can be installed by prefixing the Homebrew installation command
with `arch -x86_64 ...`. By default the x86_64 Homebrew uses the `/usr/local`
prefix while the arm64 Homebrew uses the `/opt/homebrew` prefix so both
installations can co-exist. To use the x86_64 Homebrew, e.g. to install the
x86_64 Node.js, invoke it as follows:

```
arch -x86_64 /usr/local/bin/brew install node npm
```

And then update the `PATH` environment variable to make sure the x86_64 `node` executable gets used:

```
cd integration/vscode/ada; PATH=/usr/local/bin:$PATH npm install
```

Once the initial `npm install` step is done in x86_64 mode, VS Code will be able
to use the installed x86_64 dependencies seemlessly.

## Terminology

### Syntax Highlighter

The editor component which is responsible for applying highlighting of syntax within the editor display. Information is supplied to the highlighter from the classifier or language server.

### Classifier

The extension component which is responsible for parsing and classifying tokens.

### Language Server

The extension component which is responsible for providing syntax completion and other features; it may optionally provide token classification.

## How it works

The classifier is given a JSON document which itself holds a number of "structured" Regex object. This is similar in concept to [Structural Regular Expressions](http://doc.cat-v.org/bell_labs/structural_regexps/se.pdf) but is not the same implementation.

[VS Code](https://code.visualstudio.com/) utilizes a line oriented parser. This is done for performance reasons, and is very effective to that extent. It does, however, mean that `\s` will not match a new line, and `\n` is effectively meaningless as it will never match.

Ada is a highly structured and "context sensitive" language. This does not necessarily, but can, mean a formal [Context-Sensitive Grammar](https://en.wikipedia.org/wiki/Context-sensitive_grammar), but does mean that keywords and other constructs may be best served by specific classifications based on the surrounding context.

To accommodate Ada's grammar, the classifier itself uses a structured approach, rather than the typical "shotgun" approach often taken for less grammatically complex language. This means at the highest level, classifications are `meta.*` classes of structures, and continuously "drill down" into specific tokens. The repository of structured Regex objects closely follows the [Ada Reference Manual](http://ada-auth.org/standards/rm12_w_tc1/html/RM-TOC.html) [Annex P](http://ada-auth.org/standards/rm12_w_tc1/html/RM-P.html) in both naming and definitions.

Ada's grammar is officially given in EBNF however, and structured Regex is not EBNF. Because of this and [VS Code](https://code.visualstudio.com/)'s line oriented nature, some concessions have to be made. As such, syntax definitions may not exactly match EBNF, and may use a localized "shotgun" approach at certain times. Certain keywords or constructs may need to be moved around. And there are certain places where allowing line breaks is remarkably difficult or even impossible.

## New Classifications

When supplying new classifications it is a good idea to keep everything as close to [Annex P](http://ada-auth.org/standards/rm12_w_tc1/html/RM-P.html) definitions as possible. Furthermore, check for regressions.

Furthermore, classifications within `repository` should be kept in alphabetical order.

## Final Notes

Do not hesitate to ask for advice or help. *Any* working contributions are greatly appreciated.
