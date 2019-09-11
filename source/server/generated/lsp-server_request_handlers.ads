--  Automatically generated, do not edit.

with LSP.Messages.Server_Requests;
with LSP.Messages.Server_Responses;

package LSP.Server_Request_Handlers is

   type Server_Request_Handler is limited interface;
   type Server_Request_Handler_Access is
     access all Server_Request_Handler'Class;
   --  A type which represents a handler which supports reacting
   --  to Requests. Clients implementing this interface should override
   --  the *_Request methods, and clients making use of this interface
   --  should simply call Handle_Request when they want to dispatch
   --  a Request to the handler.

   function On_Initialize_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Initialize_Request)
      return LSP.Messages.Server_Responses.Initialize_Response is abstract;

   function On_Shutdown_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Shutdown_Request)
      return LSP.Messages.Server_Responses.Shutdown_Response is abstract;

   function On_CodeAction_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.CodeAction_Request)
      return LSP.Messages.Server_Responses.CodeAction_Response is abstract;

   function On_Completion_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Completion_Request)
      return LSP.Messages.Server_Responses.Completion_Response is abstract;

   function On_Definition_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Definition_Request)
      return LSP.Messages.Server_Responses.Location_Response is abstract;

   function On_Type_Definition_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Type_Definition_Request)
      return LSP.Messages.Server_Responses.Location_Response is abstract;

   function On_Highlight_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Highlight_Request)
      return LSP.Messages.Server_Responses.Highlight_Response is abstract;

   function On_Hover_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Hover_Request)
      return LSP.Messages.Server_Responses.Hover_Response is abstract;

   function On_References_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.References_Request)
      return LSP.Messages.Server_Responses.Location_Response is abstract;

   function On_Signature_Help_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Signature_Help_Request)
      return LSP.Messages.Server_Responses.SignatureHelp_Response is abstract;

   function On_Document_Symbols_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Document_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response is abstract;

   function On_Rename_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Rename_Request)
      return LSP.Messages.Server_Responses.Rename_Response is abstract;

   function On_Execute_Command_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response is abstract;

   function On_Workspace_Symbols_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response is abstract;

   function On_Workspace_Execute_Command_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response is abstract;

   function On_ALS_Called_By_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Called_By_Request)
      return LSP.Messages.Server_Responses.ALS_Called_By_Response is abstract;

   procedure Handle_Error
     (Self  : access Server_Request_Handler) is null;
   --  This procedure will be called when an unexpected error is raised in the
   --  request processing loop.

end LSP.Server_Request_Handlers;
