# Release notes

<!-- Do not change the title of this section. After a release create a new
section below it for the last release. -->
## \<next>

* Migrate build infrastructure to ALIRE
* Migrate VSIX publication infrastructure out of GitHub Actions

## 24.0.6 (2024-06-25)

* Add Alire support for GPR language
* Take into account selection for 'sort dependencies' refactoring
* Add basic support for ALIRE in VS Code tasks
* Add tasks for GNAT SAS, GNATdoc and GNATtest
* Auto-import action for invisible completion items
* Improve hover feature for GPR files
* Run `alr show` and `alr printenv` in a sequence
* Other bug fixes
* Load GNATtest tests in the Testing view.
* Run individual tests or subsets of tests from the Testing view.

## 24.0.5 (2024-04-13)

* Display GPR errors and warnings in diagnostics
* Rework the GNATtest integration in VS Code
* Fix request traces in log files
* More requests for GPR language
  - definition
  - declaration
  - completion
* Fix tasks based on current location

## 24.0.4 (2024-02-17)

* Improve auto import suggestion
* Add all the missing workspace directories at once
* Add a 'Do not show again' button in missing dirs' popup
* Create a launch configuration for each main for attaching to a running process
* Make debug configurations have preLaunchTask fields pointing to build tasks
* Package a sourcemap file for the Ada extension
* Use case-sensitive search when renaming comments
* Handle null environment variable in vscode settings
* Implement a CodeLens provider of run and debug actions

## 24.0.3 (2023-12-07)

* Mac OS Apple M1 experimental support
* Tooltips for aspects, pragmas and attributes
* GPR LS: with clauses, Case/When statements in Symbols
* GPR LS: completion request for package & attributes
* Fix extension publishing on Marketplace

## 24.0.2 (2023-11-29)

* Experimental support on aarch64-linux platform
* Fix prepareCallHierarchy request
* Fix completion at the top of files
* Hover request in GPR files
* Depricate 'gnat' and 'gpr' tasks with 'ada' and 'spark'
* Create a parent node for with-clauses in documentSymbols request
* Organize settings into meaningful groups
* Show a popup to reload the window on environment changes

## 24.0.1 (2023-10-20)

This is the first release using the LSP 3.17 protocol.
We utilized the JSON metamodel to generate message types
and interfaces for working with them. Due to significant
changes in many types, a large portion of the code was
rewritten. Although all our tests pass, it is possible
that some errors have not yet been discovered.
Nevertheless, the new protocol will enable us to
implement even more new features for Ada in the future.

* Create a VS Code task to compile the current file
* Create a gdb debugging launch configuration
* Publish all diagnostics when refactorings fail

## 23.0.21 (2023-07-19)

* Fix VS Code extension on Windows
* Fix 'Add subprogram box' command

## 23.0.20 (2023-07-12)

* Add onTypeFormatting request initial implementation. To try edit `settings.json` with:

```json
   "[ada]": {
      "editor.formatOnType": true,
   },
   "ada.onTypeFormatting.indentOnly": false,
```

* Fixes and improvements in syntax highlighting
  * Do not apply semantic tokens to unresolved identifiers
  * Highlight 'True' and 'False' like 'null'
* Fixes and improvements in hovers
* Basic `.gpr` language support: [document symbols and diagnostics](https://github.com/AdaCore/ada_language_server/wiki/Project-file-editing)
* Support [more architectures](https://github.com/AdaCore/ada_language_server/issues/1151) and platforms in VS Code
  * Change executable location to `<arch>/<platform>/`
  * Add `arm64` as a supported architecture
  * Add initialization code that checks specific combinations of
     architectures and platforms (e.g. arm64-darwin is supported even
     though it actually uses the x64-darwin executable, will use x86_64
     target by default however)
  * But no native ALS for `arm64` is provided for now
* Accept task bodies and packages for [subprogram box command](https://github.com/AdaCore/ada_language_server/issues/1153)
* publish diagnostics when a refactoring fails.

![image](https://github.com/AdaCore/ada_language_server/assets/6430719/8b99bb79-eece-42e1-84d1-098640b7783d)

## 23.0.19 (2023-06-15)

* Fix highlights for obsolescent entities
* Fix formatting for simple aggregate
* Fix the build script to avoid unusable binaries on macOS
* Fix --version reporting

## 23.0.18 (2023-05-26)

* Better diagnostic on errors on the project loading
* Pack debug info into separate `.debug` files in GitHub Release assets
* Improvements of the SPARK support
* Clarify the message in hovers when in aggregate projects
* Add GPR tasks to build and run main subprograms

## 23.0.17 (2023-05-09)

* Increasing performance of Goto_Declaration
* Use GNATdoc for all constructs.
* Improve error messages for project loading
* Substitute any variable reference when setting process.env
* Improve aspect completion
* Improve speed by simplify Is_Ada_File

## 23.0.16 (2023-04-05)

* Add support for setTrace notification
* Fix the issue with Excluded_Source_Dirs [#1131](https://github.com/AdaCore/ada_language_server/issues/1131)
* Fix the issue formatting isse [#1133](https://github.com/AdaCore/ada_language_server/issues/1133)
* Fix the issue with typeless renaming [#1138](https://github.com/AdaCore/ada_language_server/issues/1138)

## 23.0.15 (2023-03-22)

* Fix `--config=file.json` [command option](https://github.com/AdaCore/ada_language_server/issues/1108)
* Don't call `alr` if environment has been [already set](https://github.com/AdaCore/ada_language_server/issues/1129)
* Allow extra `args` in task configurations
* Use diagnostics to report renaming collisions
* Use `alr exec -- <cmd>` in tasks if there is `alire.toml` file in the workspace
* Allow Windows-1252 charset for `.gpr` files as fallback
* Work with no GNAT in the `PATH` in Alire crates

## 23.0.14 (2023-02-20)

* Initial implementaiton of Alire support.
* Use libgpr2 to handle project files
* [Replace Type tool](https://github.com/AdaCore/ada_language_server/blob/master/doc/refactoring_tools.md#replace-type)
  initial implementation
* [Sort Dependencies tool](https://github.com/AdaCore/ada_language_server/blob/master/doc/refactoring_tools.md#sort-dependencies)
  initial implementation
* New --config option to specify a server configuration
  [#1108](https://github.com/AdaCore/ada_language_server/issues/1108)
* use ms-vscode.cpptools for debugging (maybe not released yet)

![Replace type](https://user-images.githubusercontent.com/22893717/217803466-ae5500fe-a071-4fe9-a669-24cd9c82917a.gif)

![Sort dependencies](https://user-images.githubusercontent.com/22893717/217805066-ee69e6d6-4c9e-4075-8eb6-1fca7793c428.gif)

## 23.0.13 (2023-01-25)

## 23.0.12 (2022-12-28)

* Activate snippet formatting in completion
* Dedicated command to reload the project
* Improvements in gnatpp/formatting
* Drop VS Code extension with degug information included

## 23.0.11 (2022-11-08)

* Fix CONSTRAINT_ERROR if client doesn't support all semantic tokens
* Highlight gnatdoc tags
* Completion handling of unnamed params in a call
* Rename files according to naming scheme

## 23.0.10 (2022-09-19)

VS code: Use per [workspace environment variables](https://github.com/AdaCore/ada_language_server/tree/master/integration/vscode/Code%20Samples/custom_env)
VS code: Add a walkthrough
ALS: Fixes for import package refactoring
ALS: Fixes for range formatting
ALS: Run pretty printer on completion snippets
ALS: Rename files according to project's naming scheme
ALS: Improve completion snippets in case of keywords
ALS: Add minimal doc for parameter completion

## 23.0.9 (2022-08-16)

* Make VSCode commands Ada-specific
* GNATpp partial formatting integration
* Package "Native Debug" extension with Extension Pack
* Add `subprogram box` command
* Better completion for parameters
* Avoid suggesting same code action multiple times
* Display workspace popup only if workspace file
* Introduce Parameter tool initial implementation
* Add two spaces for Ada comments
* Switch build scripts to GCC 12

## 23.0.8 (2022-07-20)

* Make [binary releases](https://github.com/AdaCore/ada_language_server/releases)
* Fix precedence of pretty printer options
* Turn on documentRangeFormattingProvider
* GNATpp partial formatting integration
* Rewrite parameter completion
* Use completion weight in sortText
* Fix completion for predefined types
* Add useCompletionSnippets flag
* Limit number of logs in $ALS_HOME/.als
* Improve highlighter robustness
* Fix project handling for alire generated projects

## 23.0.7 (2022-06-07)

* [Refactoring documentation](https://github.com/AdaCore/ada_language_server/blob/master/doc/refactoring_tools.md)
* Fix aggregate snippets
* Futher hover migration `gnatdoc` engine
* Implement semantic token range request
* Fix formatting (pretty printer) for square brackets
* Bundle JS files before packaginf `.vsix` file

## 23.0.6 (2022-05-02)

* New refactoring: Change Parameter Type
* New refactoring: Change Parameter Default Value
* New refactoring: Pull Up Declaration
* New refactoring: Extract Subprogram
* Fix Call Hierarchy requests
* First steps with introducing GNATdoc for hovers
* New tasks for GNATprove integration
* Fix for aggregate completion
* Semantic tokens highlighter
* [Custom colors in VS Code](https://github.com/AdaCore/ada_language_server/wiki/Custom-colors-in-VS-Code)

![Screenshot](https://user-images.githubusercontent.com/6430719/165566312-4bb5ac9f-f362-4331-906a-eee98d1a11fe.jpg)

## 23.0.5 (2022-04-05)

* Pull Up Declaration initial implementation
* New tasks to Prove line/region with gnatprove
* Some bug fixes for completion

[![Pull Up declaration video](https://user-images.githubusercontent.com/6430719/161923655-ac60df1d-7c59-407c-9bb1-c6912f9e4fd9.png)](https://user-images.githubusercontent.com/6430719/161922995-6eb4211f-63e8-4b74-bdcd-d49f1d1f4248.mp4)

## 23.0.4 (2022-03-08)

* Initial implementation of Extract Subprogram Refactoring
* Signature help improvements
* Fixes in attribute and aspect completion
* Display gnatpp messages if any

## 23.0.3 (2022-02-01)

* Recompute predefined tasks when scenario vars change
* Fix stale_reference_error exceptions once again
* A new completion provider for "end names"
* Add a VS Code task for gprclean
* A stub for GPR language server

## 23.0.2 (2021-12-01)

* Fix some completion issues
* Implement cancel for FoldingRange request
* Use macos-10.15/ubuntu-18.04 for building on GH
* Do not raise errors for missing object dirs
* A new `Add_Parameter` refactoring tool

## 23.0.1 (2021-10-25)

* Avoid duplicates in completion
* Implement partial response for Workspace_Symbols request
* Better completion for parameters
* A build task provider for gnatprove
* Use GCC 11 by alire to build the extension
* File renaming of top level decls
* Special case in completion for end names
* Сompletion for attributes
* New "Add Parameter" refactoring tool

## 22.0.11 (2021-09-16)

* Remove "Imprecise fallback" popup
* Use line terminator of the document instead of hardcoded '\n'
* Increase stack size to prevent craches on deep nesting calls in LAL
* Index runtime files at startup for better completions
* Compute some completion properties lazily
* Filter the signatures using the previous designators
* Fix optional capabilities parsing for some clients (e.g. emacs)
* Add project loading diagnostics and code actions to help fixing
* Don't insert 'invisible' in subprogram snippets

## 22.0.10 (2021-08-25)

* Implement library unit renames!
* Fix ALS executable name on Windows
* Fix errors when there are TABs and some Unicode in sources
* Fix issues with signature help requests

## 22.0.9 (2021-08-09)

* Add a [Tutorial](https://github.com/AdaCore/ada_language_server/wiki/Getting-Started)
* Switch build to GNAT CE 2021 for Linux and Windows
* Improve support for files detached from a project
* Fixes for invisible symbol completion
* More entities in Workspace Symbols
* Fix signatureHelp for null subps and exprfuncs
* Set filterText for invisible symbols completion
* Fix snippet indentation characters
* Fix hightlight rules for `body` patterns

## 22.0.8 (2021-06-09)

* Add a command to switch between spec and body files.
* Add Suppress Separate refactoring tool
* Update name of 'refactoring_rename' tool
* Silence notification for imprecise navigation
* Fix signatureHelp active parameter + Prefix notation
* Fix errors on win32 with case-insensitive URIs
* Fix refactoring import capatibility
* Change implementation of callHierarchy requests

## 22.0.7 (2021-04-27)

* The signatureHelp support
* A fix for STALE_REFERENCE_ERROR
* Improvements/fixes for callHierarchy
* A fix for Change Subp Signatures refactorings

## 22.0.6 (2021-04-01)

* Task provider for "build project"/"check file" tasks
* Named Parameters refactoring for more constructs
* Add basic support for preprocessor directives
* Add declaration in textDocument/Highlight results
* Fix symlink issue and a new followSymlinks setting

## 22.0.5 (2021-03-12)

* Add 3 subprogram signature refactoring tools:
  - Remove Parameter
  - Change Parameter Mode
  - Move Parameter Left/Right
* Suppress more LAL exception raised on invalid code.

## 22.0.4 (2021-02-13)

* Use client side file monitoring if the client provides it.
* Improve refactoring rename
* Use iterators for completion results could prevent stack overflow on
  a large completion list

## 22.0.3 (2020-12-14)

* Improved safety for entity renaming
* Call hierarchy follows renaming declarations
* Workspace symbols and completion don't display local symbols any more
* Use static linking for the server

## 22.0.2 (2020-11-26)

* Implementation of `textDocument/highlight`
* filesystem monitoring (off by default)

## 22.0.1 (2020-10-12)

* Implement `textDocument/prepareRename` request
* New `relocateBuildTree` and `rootDir` configuration options
* Changes in `textDocument/declaration` behavior
* New a 'displayMethodAncestryOnNavigation' configuration option

## 21.0.17 (2020-09-22)

* Add call hierarchy request from LSP 3.16
* New Code Action to insert with-clause and a prefix
  to an unresolved symbol in the editor.
* Show high level pragma in documentSymbols
* Dedicated README.md for vscode extension
* Change in the "Go to definition": List the overriding
  subprograms only if we are clicking on a 'usage' name
* Add highlighting for gnatprep preprocessor directives

## 21.0.16 (2020-08-17)

* Implementation of workspace/symbol request
* Mark completion with '(invisible)' text if needed
* Minor fixes in imprecise navigation and completion

## 21.0.15 (2020-08-07)

* Restore advanced syntax highlighter.
* Documentation for invisible symbol completions
* Separate variables and constants in documentSymbol
* Fix mode position in subprogram snippets

## 21.0.14 (2020-07-15)

* Completion for invisible symbols with a lower priority
* Completion for keywords, aspects, pragmas and attributes.
* Find access references in dotted name expressions
* Use Pretty_Printer switches from a project file for formatting

## 21.0.13 (2020-07-01)

* Minor fixex for hover, completion, etc
* Add foldComments setting
* Completion for aggregate of derived types.
* Faster JSON codecs
* Use incremental text changes

## 21.0.12 (2020-05-26)

* Implementation document formatting request
* Handle aggregates for completion

## 21.0.11 (2020-05-15)

* Revert for basic syntax highlighter for a while
* Some tuning in DocumentSymbols request
* Improvements in completion
* Display project name for aggregate project in hovers

## 21.0.10 (2020-04-07)

* Improve rename in comments
* Minor fixes in folding
* Syntax highlighter refactoring
* Use case-insensitve check to filter completion
* Add documentation for completion items
* Enable work progress report for indexing
* Fix behavior when encountering an invalid project
* Add completion for subprogram parameters

## 21.0.9 (2020-03-13)

Last changes:

* Move the extension to the new Marketplace account.

## 21.0.8 (2020-03-13)

We are going to move ALS extension to our new [corporative Marketplace account](https://marketplace.visualstudio.com/publishers/AdaCore).

Please, uninstall the extension and then launch VS Code Quick Open (Ctrl+P), paste the following command, and press enter.

```plain
ext install AdaCore.ada
```

Last changes:

* Some syntax rules reorganization
* Improve folding for some constructs and comments
* Filter completion results with typed prefix
* Fix named parameter refactoring issues

## 21.0.7 (2020-03-02)

* Initial implementation of the folding request
* Announce implementationProvider during initialization

## 21.0.6 (2020-02-13)

* Advanced syntax highlighter for Ada by Patrick Kelly
* Basic highlighting for *.ali files by Patrick Kelly also
* More Ada snippets and new .gpr snippets

## 21.0.5 (2020-02-07)

* Expirimental support of receiving incremental editor changes
* Hover responses for is abstract/null subprogram
* Implementation of "named parameters" refactoring
* Fix identifier highlightштп in vscode extension
* Consider runtime files as part of project files
* load dirs on demand when the project is implicit
* Fix Assetion_Error sor some `hover` request

## 21.0.4 (2020-01-23)

* For `textDocument/documentSymbol` return a hierarchy of symbols
  if the client supports it.
* Don't raise an exception on unknown method, return corresponing
  erro code instead.
* Rename parameters in the whole hierarchy.
  When renaming a parameter of a tagged type primitive, we now
  also rename this parameter in all the base and overriding
  subprograms.
* Don't send `executeCommandProvider` and `documentLinkProvider`,
  because the server doesn't implement such requests.
* Remove performance bottleneck.
* Add `child` and `reference` ALS reference kinds(an als extension).
* Improve `textDocument/typeDefinition` to work with more declaration
  kinds (not just with object_declaration).
* Improve project handling.
  In the case that no project is specified, use an implicit
  project which covers the subdirectories as well. This is slightly
  friendlier for VScode users who don't have a .gpr at the root
  of a hierarchy.

## 21.0.3 (2019-12-20)

* The `textDocument/declaration` request is now implemented.
* The 'textDocument/implementation' request is now implemented.
* Find all references doesn't return 'end label' any more.
* Hover includes aspect clauses for subprogram declaration.
* The `textDocument/definition` request returns overrided and overriding
  declarations.
* The `textDocument/rename` request renames the overrided and overriding
  subprograms.

## 21.0.2 (2019-11-14)

* Add Problem Matcher
* Rebase libadalang to 13a95e7af79c94c0ec830052fbf6aa06e90eefae
* Add support for the `$/progress` notification from LSP v3.15
* Add a fallback mechanism for `definition`
* Allow "called_by" on an abstract definition
* Don't return error responses when Property_Error is raised
* Suppress noisy "loading"/"done loading" messages

## 21.0.1 (2019-10-16)

* Suppress noisy "loading"/"done loading" messages.
* Don't return error responses when Property_Error is raised in Libadalang.
* Improve testsuite.
