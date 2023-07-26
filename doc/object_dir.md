# Object Directory

## Short introduction

This is a custom request used by the VS Code extension to retrieve the Object Directory from the GPR project file, allowing us to get the path to the object directory currently in use.

## Change description

We introduce a new type to represent the request results:

```typescript

type ObjDirResponse = {
    Value : string;
};

```

And a new request:

  method: `$/glsObjectDir`
  params: null

Returning the project file of the loaded project like this:

  result: `ObjDirResponse`
