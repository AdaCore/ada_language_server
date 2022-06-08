# Source dirs

## Short introduction

This is a custom request used by the VS Code extension to retrieve the project's source directories, allowing to add missing directories in the workspace when needed.

## Change description

We introduce three new types to represent the request parameters and results:

```typescript

interface ALS_Unit_Description {
   name       : string; /* The source directory's name in VS Code */
   uri : DocumentUri;   /* The source directory's uri */
}
```

And a new request:

  method: `workspace/alsSourceDirs`
  params: null

Returning the source directories of the loaded project like this:

  result: `ALS_Source_Dir_Description[]`
