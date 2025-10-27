# Global/Local Variables highlighting

## Short introduction

This feature uses custom modifiers for `SemanticTokenModifiers` to mark **global** and **local** variables in the `semanticTokens` request.

## Change description

We extend the `SemanticTokenModifiers` by adding extra modifiers:

* `globalVariable`: Marks a **global variable**. This refers to a variable whose **lifetime is the same as the program itself** (program-level visibility and duration). These variables are typically declared in the declarative part of a library-level package:
    * **Library-level packages**.
    * **Nested packages**, **Protected Objects** or **Tasks** that are themselves declared at the library level. .
    
    The variable exists for the entire program execution and is not local to any locally declared subprogram or block.

* `localVariable`: Marks a **local variable**. This refers to a variable whose name is declared within the **nearest enclosing declarative part** (e.g., within a subprogram body, a block statement). Their scope and lifetime are typically restricted to that specific program element.
