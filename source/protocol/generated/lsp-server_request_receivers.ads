--  Automatically generated, do not edit.

limited with LSP.Messages.Server_Requests;

package LSP.Server_Request_Receivers is

   type Server_Request_Receiver is limited interface;
   type Server_Request_Receiver_Access is
     access all Server_Request_Receiver'Class;
   --  A type which represents a handler which supports reacting
   --  to Requests. Clients implementing this interface should override
   --  the *_Request methods, and clients making use of this interface
   --  should simply call corresponding method when they want to dispatch
   --  a Request to the handler.

   procedure On_Initialize_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Initialize_Request)
        is abstract;

   procedure On_Shutdown_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Shutdown_Request)
        is abstract;

   procedure On_CodeAction_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.CodeAction_Request)
        is abstract;

   procedure On_Completion_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Completion_Request)
        is abstract;

   procedure On_CompletionItemResolve_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.CompletionItemResolve_Request)
        is abstract;

   procedure On_Definition_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Definition_Request)
        is abstract;

   procedure On_Declaration_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Declaration_Request)
        is abstract;

   procedure On_Implementation_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Implementation_Request)
        is abstract;

   procedure On_Type_Definition_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Type_Definition_Request)
        is abstract;

   procedure On_Highlight_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Highlight_Request)
        is abstract;

   procedure On_Hover_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Hover_Request)
        is abstract;

   procedure On_Document_Links_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Document_Links_Request)
        is abstract;

   procedure On_References_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.References_Request)
        is abstract;

   procedure On_Signature_Help_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Signature_Help_Request)
        is abstract;

   procedure On_Document_Symbols_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Document_Symbols_Request)
        is abstract;

   procedure On_Rename_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Rename_Request)
        is abstract;

   procedure On_Prepare_Rename_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Prepare_Rename_Request)
        is abstract;

   procedure On_Execute_Command_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Execute_Command_Request)
        is abstract;

   procedure On_Document_Color_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Document_Color_Request)
        is abstract;

   procedure On_Color_Presentation_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Color_Presentation_Request)
        is abstract;

   procedure On_Folding_Range_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Folding_Range_Request)
        is abstract;

   procedure On_Formatting_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Formatting_Request)
        is abstract;

   procedure On_Range_Formatting_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Range_Formatting_Request)
        is abstract;

   procedure On_Selection_Range_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Selection_Range_Request)
        is abstract;

   procedure On_Document_Tokens_Full_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Document_Tokens_Full_Request)
        is abstract;

   procedure On_Document_Tokens_Range_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Document_Tokens_Range_Request)
        is abstract;

   procedure On_Prepare_Call_Hierarchy_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Prepare_Call_Hierarchy_Request)
        is abstract;

   procedure On_Incoming_Calls_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Incoming_Calls_Request)
        is abstract;

   procedure On_Outgoing_Calls_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Outgoing_Calls_Request)
        is abstract;

   procedure On_Workspace_Symbols_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
        is abstract;

   procedure On_Workspace_Execute_Command_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
        is abstract;

   procedure On_Workspace_Will_Create_Files_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Workspace_Will_Create_Files_Request)
        is abstract;

   procedure On_Workspace_Will_Rename_Files_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Workspace_Will_Rename_Files_Request)
        is abstract;

   procedure On_Workspace_Will_Delete_Files_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.Workspace_Will_Delete_Files_Request)
        is abstract;

   procedure On_ALS_Show_Dependencies_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.ALS_Show_Dependencies_Request)
        is abstract;

   procedure On_ALS_Source_Dirs_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.ALS_Source_Dirs_Request)
        is abstract;

   procedure On_ALS_Debug_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.ALS_Debug_Request)
        is abstract;

   procedure On_ALS_Check_Syntax_Request
     (Self  : access Server_Request_Receiver;
      Value : LSP.Messages.Server_Requests.ALS_Check_Syntax_Request)
        is abstract;

end LSP.Server_Request_Receivers;
