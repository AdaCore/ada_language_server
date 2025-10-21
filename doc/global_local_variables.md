# Global/local variables

## Short introduction

This feature uses custom modifiers for SemanticTokenModifiers to mark
 global/local variable in the semanticTokens request.


## Change description

We extend the `SemanticTokenModifiers` by adding extra modifiers:
  - "globalVariable" marks global variable
  - "localVariable" marks local variable
