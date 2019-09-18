# ALS Debug

## Short introduction

This request is used to test language server.

## Change description

Currently debug request has only one kind:

* Suspend_Execution - stop processing task until input queue has given
number of messages. After getting this request ALS stops message
processing, but still accepts new requests/notifications. Once
number of input messages reaches given limit, ALS resumes message
processing.

New request:
* Method: `$/alsDebug`
* params: `AlsDebugParams` defined as follows:

```typescript
interface AlsDebugSuspendExecution {
    inputQueueLength : number;  /* Min input queue length */
}

export type AlsDebugParams = AlsDebugSuspendExecution;

```

Response:
* result: `null`
