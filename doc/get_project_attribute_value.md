# Get Project Attribute Value

## Short introduction

This is a custom command used by the VS Code extension to retrieve the value of a project attribute for the
currently loaded project tree.

## Change description

The command's name is the following:

  method: `als-get-project-attribute-value`

With the following arguments:

```typescript

type ProjectAttributeValueArgs = {
   attribute: string, /* The name of the attribute (e.g: 'Main') */
   pkg: string="", /* The name of the package (e.g: 'Compiler'). Can be empty for top-level attributes */
   index: string="" /* Index for indexed attributes (e.g: 'Ada' in 'for Default_Switches ("Ada") use ..') */,
}
```

with the associated return type:

```typescript

type ProjectValueResponse = string | string[];

```
