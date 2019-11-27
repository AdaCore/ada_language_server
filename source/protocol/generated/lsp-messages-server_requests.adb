--  Automatically generated, do not edit.

package body LSP.Messages.Server_Requests is

   --  These messages are sent from client to server.

   overriding procedure Visit
     (Self    : Initialize_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Initialize_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Shutdown_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Shutdown_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : CodeAction_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_CodeAction_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Completion_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Completion_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Definition_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Definition_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Declaration_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Declaration_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Type_Definition_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Type_Definition_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Highlight_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Highlight_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Hover_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Hover_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : References_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_References_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Signature_Help_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Signature_Help_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Document_Symbols_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Document_Symbols_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Rename_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Rename_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Execute_Command_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Execute_Command_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Workspace_Symbols_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Workspace_Symbols_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Workspace_Execute_Command_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Workspace_Execute_Command_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : ALS_Called_By_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_ALS_Called_By_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : ALS_Debug_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_ALS_Debug_Request (Self);
   end Visit;

end LSP.Messages.Server_Requests;
