# CallHierarchyOutgoingCall / CallHierrachyIncomingCall

## Short introduction

The response types to the `callHierarchy/outgoingCalls` and `callHierarchy/incomingCalls` LSP requests
have been customized in order to distinguish dispatching calls from non-dispatching ones.


## Change description

   Here is the modified version of the `CallHierarchyIncomingCall` LSP type:

   ```typescript
   export interface CallHierarchyIncomingCall {

     /**
      * The item that makes the call.
      */
     from: CallHierarchyItem;

     /**
      * The ranges at which the calls appear. This is relative to the caller
      * denoted by [`this.from`](#CallHierarchyIncomingCall.from).
      */
     fromRanges: Range[];

     /**
      * Used to know which call listed in `fromRanges` is dispatching or not.
        The array length is equal to the `fromRanges` ones.
      */
     dispatching_calls?: Boolean[];
   }
   ```

   Here is the modified version of the `CallHierarchyOutgoingCall` LSP type:

   ```typescript
   export interface CallHierarchyOutgoingCall {

     /**
      * The item that is called.
      */
     to: CallHierarchyItem;

     /**
      * The range at which this item is called. This is the range relative to
      * the caller, e.g the item passed to `callHierarchy/outgoingCalls` request.
      */
     fromRanges: Range[];

     /**
      * Used to know which call listed in `fromRanges` is dispatching or not.
        The array length is equal to the `fromRanges` ones.
      */
     dispatching_calls?: Boolean[];
   }
   ```
