--  Automatically generated, do not edit.

package body LSP.Messages.Server_Requests is

   --  These messages are sent from client to server.

   Map : Maps.Map;

   function Method_To_Tag
     (Method : VSS.Strings.Virtual_String) return Ada.Tags.Tag is
   begin
      return Method_To_Tag (Map, Method);
   end Method_To_Tag;

   overriding procedure Visit
     (Self    : Initialize_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Initialize_Request (Self);
   end Visit;

   overriding function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
      return Shutdown_Request is
   begin
      return V : Shutdown_Request do
         RequestMessage'Read (JS, RequestMessage (V));
      end return;
   end Decode;

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
     (Self    : CompletionItemResolve_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_CompletionItemResolve_Request (Self);
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
     (Self    : Implementation_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Implementation_Request (Self);
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
     (Self    : Document_Links_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Document_Links_Request (Self);
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
     (Self    : Prepare_Rename_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Prepare_Rename_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Execute_Command_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Execute_Command_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Document_Color_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Document_Color_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Color_Presentation_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Color_Presentation_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Folding_Range_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Folding_Range_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Formatting_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Formatting_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Range_Formatting_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Range_Formatting_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Selection_Range_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Selection_Range_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Document_Tokens_Full_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Document_Tokens_Full_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Document_Tokens_Range_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Document_Tokens_Range_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Prepare_Call_Hierarchy_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Prepare_Call_Hierarchy_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Incoming_Calls_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Incoming_Calls_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Outgoing_Calls_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Outgoing_Calls_Request (Self);
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
     (Self    : Workspace_Will_Create_Files_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Workspace_Will_Create_Files_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Workspace_Will_Rename_Files_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Workspace_Will_Rename_Files_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : Workspace_Will_Delete_Files_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_Workspace_Will_Delete_Files_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : ALS_Show_Dependencies_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_ALS_Show_Dependencies_Request (Self);
   end Visit;

   overriding function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
      return ALS_Source_Dirs_Request is
   begin
      return V : ALS_Source_Dirs_Request do
         RequestMessage'Read (JS, RequestMessage (V));
      end return;
   end Decode;

   overriding procedure Visit
     (Self    : ALS_Source_Dirs_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_ALS_Source_Dirs_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : ALS_Debug_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_ALS_Debug_Request (Self);
   end Visit;

   overriding procedure Visit
     (Self    : ALS_Check_Syntax_Request;
      Handler : access Server_Request_Receiver'Class) is
   begin
      Handler.On_ALS_Check_Syntax_Request (Self);
   end Visit;

begin

   Map.Insert
     ("initialize",
      Initialize_Request'Tag);

   Map.Insert
     ("shutdown",
      Shutdown_Request'Tag);

   Map.Insert
     ("textDocument/codeAction",
      CodeAction_Request'Tag);

   Map.Insert
     ("textDocument/completion",
      Completion_Request'Tag);

   Map.Insert
     ("completionItem/resolve",
      CompletionItemResolve_Request'Tag);

   Map.Insert
     ("textDocument/definition",
      Definition_Request'Tag);

   Map.Insert
     ("textDocument/declaration",
      Declaration_Request'Tag);

   Map.Insert
     ("textDocument/implementation",
      Implementation_Request'Tag);

   Map.Insert
     ("textDocument/typeDefinition",
      Type_Definition_Request'Tag);

   Map.Insert
     ("textDocument/documentHighlight",
      Highlight_Request'Tag);

   Map.Insert
     ("textDocument/hover",
      Hover_Request'Tag);

   Map.Insert
     ("textDocument/documentLink",
      Document_Links_Request'Tag);

   Map.Insert
     ("textDocument/references",
      References_Request'Tag);

   Map.Insert
     ("textDocument/signatureHelp",
      Signature_Help_Request'Tag);

   Map.Insert
     ("textDocument/documentSymbol",
      Document_Symbols_Request'Tag);

   Map.Insert
     ("textDocument/rename",
      Rename_Request'Tag);

   Map.Insert
     ("textDocument/prepareRename",
      Prepare_Rename_Request'Tag);

   Map.Insert
     ("textDocument/executeCommand",
      Execute_Command_Request'Tag);

   Map.Insert
     ("textDocument/documentColor",
      Document_Color_Request'Tag);

   Map.Insert
     ("textDocument/colorPresentation",
      Color_Presentation_Request'Tag);

   Map.Insert
     ("textDocument/foldingRange",
      Folding_Range_Request'Tag);

   Map.Insert
     ("textDocument/formatting",
      Formatting_Request'Tag);

   Map.Insert
     ("textDocument/rangeFormatting",
      Range_Formatting_Request'Tag);

   Map.Insert
     ("textDocument/selectionRange",
      Selection_Range_Request'Tag);

   Map.Insert
     ("textDocument/semanticTokens/full",
      Document_Tokens_Full_Request'Tag);

   Map.Insert
     ("textDocument/semanticTokens/range",
      Document_Tokens_Range_Request'Tag);

   Map.Insert
     ("textDocument/prepareCallHierarchy",
      Prepare_Call_Hierarchy_Request'Tag);

   Map.Insert
     ("callHierarchy/incomingCalls",
      Incoming_Calls_Request'Tag);

   Map.Insert
     ("callHierarchy/outgoingCalls",
      Outgoing_Calls_Request'Tag);

   Map.Insert
     ("workspace/symbol",
      Workspace_Symbols_Request'Tag);

   Map.Insert
     ("workspace/executeCommand",
      Workspace_Execute_Command_Request'Tag);

   Map.Insert
     ("workspace/willCreateFiles",
      Workspace_Will_Create_Files_Request'Tag);

   Map.Insert
     ("workspace/willRenameFiles",
      Workspace_Will_Rename_Files_Request'Tag);

   Map.Insert
     ("workspace/willDeleteFiles",
      Workspace_Will_Delete_Files_Request'Tag);

   Map.Insert
     ("textDocument/alsShowDependencies",
      ALS_Show_Dependencies_Request'Tag);

   Map.Insert
     ("workspace/alsSourceDirs",
      ALS_Source_Dirs_Request'Tag);

   Map.Insert
     ("$/alsDebug",
      ALS_Debug_Request'Tag);

   Map.Insert
     ("$/alsCheckSyntax",
      ALS_Check_Syntax_Request'Tag);
end LSP.Messages.Server_Requests;
