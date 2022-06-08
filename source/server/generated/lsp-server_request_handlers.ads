--  Automatically generated, do not edit.

pragma Style_Checks (Off);

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
      return LSP.Messages.Server_Responses.Initialize_Response
        is abstract;

   function On_Shutdown_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Shutdown_Request)
      return LSP.Messages.Server_Responses.Shutdown_Response
        is abstract;

   function On_CodeAction_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.CodeAction_Request)
      return LSP.Messages.Server_Responses.CodeAction_Response
        is abstract;

   function On_Completion_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Completion_Request)
      return LSP.Messages.Server_Responses.Completion_Response
        is abstract;

   function On_CompletionItemResolve_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.CompletionItemResolve_Request)
      return LSP.Messages.Server_Responses.CompletionItemResolve_Response
        is abstract;

   function On_Definition_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
        is abstract;

   function On_Declaration_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Declaration_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
        is abstract;

   function On_Implementation_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Implementation_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
        is abstract;

   function On_Type_Definition_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Type_Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
        is abstract;

   function On_Highlight_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Highlight_Request)
      return LSP.Messages.Server_Responses.Highlight_Response
        is abstract;

   function On_Hover_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Hover_Request)
      return LSP.Messages.Server_Responses.Hover_Response
        is abstract;

   function On_Document_Links_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Document_Links_Request)
      return LSP.Messages.Server_Responses.Links_Response
        is abstract;

   function On_References_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.References_Request)
      return LSP.Messages.Server_Responses.Location_Response
        is abstract;

   function On_Signature_Help_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Signature_Help_Request)
      return LSP.Messages.Server_Responses.SignatureHelp_Response
        is abstract;

   function On_Document_Symbols_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Document_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response
        is abstract;

   function On_Rename_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Rename_Request)
      return LSP.Messages.Server_Responses.Rename_Response
        is abstract;

   function On_Prepare_Rename_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Prepare_Rename_Request)
      return LSP.Messages.Server_Responses.Prepare_Rename_Response
        is abstract;

   function On_Execute_Command_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
        is abstract;

   function On_Document_Color_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Document_Color_Request)
      return LSP.Messages.Server_Responses.DocumentColor_Response
        is abstract;

   function On_Color_Presentation_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Color_Presentation_Request)
      return LSP.Messages.Server_Responses.ColorPresentation_Response
        is abstract;

   function On_Folding_Range_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Folding_Range_Request)
      return LSP.Messages.Server_Responses.FoldingRange_Response
        is abstract;

   function On_Formatting_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Formatting_Request)
      return LSP.Messages.Server_Responses.Formatting_Response
        is abstract;

   function On_Range_Formatting_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Range_Formatting_Request)
      return LSP.Messages.Server_Responses.Range_Formatting_Response
        is abstract;

   function On_Selection_Range_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Selection_Range_Request)
      return LSP.Messages.Server_Responses.SelectionRange_Response
        is abstract;

   function On_Document_Tokens_Full_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Full_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response
        is abstract;

   function On_Document_Tokens_Range_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Range_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response
        is abstract;

   function On_Prepare_Call_Hierarchy_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Prepare_Call_Hierarchy_Request)
      return LSP.Messages.Server_Responses.PrepareCallHierarchy_Response
        is abstract;

   function On_Incoming_Calls_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Incoming_Calls_Request)
      return LSP.Messages.Server_Responses.IncomingCalls_Response
        is abstract;

   function On_Outgoing_Calls_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Outgoing_Calls_Request)
      return LSP.Messages.Server_Responses.OutgoingCalls_Response
        is abstract;

   function On_Workspace_Symbols_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response
        is abstract;

   function On_Workspace_Execute_Command_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
        is abstract;

   function On_Workspace_Will_Create_Files_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Will_Create_Files_Request)
      return LSP.Messages.Server_Responses.WillCreateFiles_Response
        is abstract;

   function On_Workspace_Will_Rename_Files_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Will_Rename_Files_Request)
      return LSP.Messages.Server_Responses.WillRenameFiles_Response
        is abstract;

   function On_Workspace_Will_Delete_Files_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Will_Delete_Files_Request)
      return LSP.Messages.Server_Responses.WillDeleteFiles_Response
        is abstract;

   function On_ALS_Show_Dependencies_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Show_Dependencies_Request)
      return LSP.Messages.Server_Responses.ALS_ShowDependencies_Response
        is abstract;

   function On_ALS_Source_Dirs_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Source_Dirs_Request)
      return LSP.Messages.Server_Responses.ALS_SourceDirs_Response
        is abstract;

   function On_ALS_Debug_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Debug_Request)
      return LSP.Messages.Server_Responses.ALS_Debug_Response
        is abstract;

   function On_ALS_Check_Syntax_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Check_Syntax_Request)
      return LSP.Messages.Server_Responses.ALS_Check_Syntax_Response
        is abstract;

   procedure Handle_Error
     (Self  : access Server_Request_Handler) is null;
   --  This procedure will be called when an unexpected error is raised in the
   --  request processing loop.

end LSP.Server_Request_Handlers;
