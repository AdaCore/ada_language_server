# Check Syntax

## Short introduction

This implements a functionality to query if a given input has a valid syntax according to a set of rules.

## Change description

We introduce a new request:

    method: `alsCheckSyntax`

And two types to represent the request and response parameters:

```typescript
type AdaSyntaxCheckRequest = {
    input: string;
    rules: string[];
};

type AdaSyntaxCheckResponse = {
    diagnostic?: string;
};
```

`rules` is a `string[]` and its content must be Libadalang `Ada_Node_Kind_Type` values.