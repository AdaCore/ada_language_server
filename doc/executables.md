# Executables

## Short introduction

This implements a functionality to query the mains and the executables for a multi targets project.

## Capabilities

We provide the Build and Run tasks for specific targets in a GPR Projects.

To check these tasks :
- click :  `Ctrl + Shift + P `.
- Select  `Run Tasks` , then Select  `GPR Tasks`.

## Change description

We introduce two commands, the first one:

    command: `als-mains`

Which provides the mains for the project, with a response type:

```typesript
type GlsMainResult = string[];
```

The second one is:

    command: `als-executables`

Which provides the executables for the project, with a response type:

```typesript
type GlsExecutableResult = string[];
```
