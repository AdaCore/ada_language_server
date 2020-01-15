--  Automatically generated, do not edit.

with Ada.Strings.UTF_Encoding;
with LSP.Messages.Common_Writers;

package body LSP.Messages.Server_Requests is

   --  These messages are sent from client to server.

   Map : Maps.Map;

   function Method_To_Tag
     (Method : LSP.Types.LSP_String) return Ada.Tags.Tag is
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
         Messages.Common_Writers.Set_Common_Request_Fields (V, JS.all);
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

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

begin

   Map.Insert
     (+"initialize",
      Initialize_Request'Tag);

   Map.Insert
     (+"shutdown",
      Shutdown_Request'Tag);

   Map.Insert
     (+"textDocument/codeAction",
      CodeAction_Request'Tag);

   Map.Insert
     (+"textDocument/completion",
      Completion_Request'Tag);

   Map.Insert
     (+"textDocument/definition",
      Definition_Request'Tag);

   Map.Insert
     (+"textDocument/declaration",
      Declaration_Request'Tag);

   Map.Insert
     (+"textDocument/implementation",
      Implementation_Request'Tag);

   Map.Insert
     (+"textDocument/typeDefinition",
      Type_Definition_Request'Tag);

   Map.Insert
     (+"textDocument/highight",
      Highlight_Request'Tag);

   Map.Insert
     (+"textDocument/hover",
      Hover_Request'Tag);

   Map.Insert
     (+"textDocument/documentLink",
      Document_Links_Request'Tag);

   Map.Insert
     (+"textDocument/references",
      References_Request'Tag);

   Map.Insert
     (+"textDocument/signatureHelp",
      Signature_Help_Request'Tag);

   Map.Insert
     (+"textDocument/documentSymbol",
      Document_Symbols_Request'Tag);

   Map.Insert
     (+"textDocument/rename",
      Rename_Request'Tag);

   Map.Insert
     (+"textDocument/executeCommand",
      Execute_Command_Request'Tag);

   Map.Insert
     (+"textDocument/documentColor",
      Document_Color_Request'Tag);

   Map.Insert
     (+"workspace/symbol",
      Workspace_Symbols_Request'Tag);

   Map.Insert
     (+"workspace/executeCommand",
      Workspace_Execute_Command_Request'Tag);

   Map.Insert
     (+"textDocument/alsCalledBy",
      ALS_Called_By_Request'Tag);

   Map.Insert
     (+"$/alsDebug",
      ALS_Debug_Request'Tag);
end LSP.Messages.Server_Requests;
