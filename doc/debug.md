# ALS Debug

## Short introduction

This command `als-suspend-execution` is used to test language server.

## Change description

Currently `als-suspend-execution` debug command has only one argument:

* inputQueueLength - integer

On execution it stops processing task until input queue has given
number of messages. After getting this request ALS stops message
processing, but still accepts new requests/notifications. Once
number of input messages reaches given limit, ALS resumes message
processing.

Response:
* result: `null`
