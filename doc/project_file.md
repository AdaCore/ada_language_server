# Project File

## Short introduction

This is a custom command used by the VS Code extension to retrieve the GPR project file uploaded by the ALS, allowing us to get the path to the project file currently in use.

## Change description

We introduce a new type to represent the request results:

```typescript

export type ProjectFileResponse = string; // The Path to the GPR project file
```

And a new command with out arguments:

  command: `als-project-file`

Returning the project file of the loaded project like this:

  result: `ProjectFileResponse`
