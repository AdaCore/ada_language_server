--  Automatically generated, do not edit.

with LSP.Generic_Requests;

package LSP.Messages.Server_Requests is

   package Initialize_Requests is
     new LSP.Generic_Requests (InitializeParams);

   type Initialize_Request is
     new Initialize_Requests.Request with null record;

   type Shutdown_Request is new RequestMessage with null record;

   package CodeAction_Requests is
     new LSP.Generic_Requests (CodeActionParams);

   type CodeAction_Request is
     new CodeAction_Requests.Request with null record;

   package Completion_Requests is
     new LSP.Generic_Requests (TextDocumentPositionParams);

   type Completion_Request is
     new Completion_Requests.Request with null record;

   package Definition_Requests is
     new LSP.Generic_Requests (TextDocumentPositionParams);

   type Definition_Request is
     new Definition_Requests.Request with null record;

   package Type_Definition_Requests is
     new LSP.Generic_Requests (TextDocumentPositionParams);

   type Type_Definition_Request is
     new Type_Definition_Requests.Request with null record;

   package Highlight_Requests is
     new LSP.Generic_Requests (TextDocumentPositionParams);

   type Highlight_Request is
     new Highlight_Requests.Request with null record;

   package Hover_Requests is
     new LSP.Generic_Requests (TextDocumentPositionParams);

   type Hover_Request is
     new Hover_Requests.Request with null record;

   package References_Requests is
     new LSP.Generic_Requests (ReferenceParams);

   type References_Request is
     new References_Requests.Request with null record;

   package Signature_Help_Requests is
     new LSP.Generic_Requests (TextDocumentPositionParams);

   type Signature_Help_Request is
     new Signature_Help_Requests.Request with null record;

   package Document_Symbols_Requests is
     new LSP.Generic_Requests (DocumentSymbolParams);

   type Document_Symbols_Request is
     new Document_Symbols_Requests.Request with null record;

   package Rename_Requests is
     new LSP.Generic_Requests (RenameParams);

   type Rename_Request is
     new Rename_Requests.Request with null record;

   package Execute_Command_Requests is
     new LSP.Generic_Requests (ExecuteCommandParams);

   type Execute_Command_Request is
     new Execute_Command_Requests.Request with null record;

   package Workspace_Symbols_Requests is
     new LSP.Generic_Requests (WorkspaceSymbolParams);

   type Workspace_Symbols_Request is
     new Workspace_Symbols_Requests.Request with null record;

   package Workspace_Execute_Command_Requests is
     new LSP.Generic_Requests (ExecuteCommandParams);

   type Workspace_Execute_Command_Request is
     new Workspace_Execute_Command_Requests.Request with null record;

   package ALS_Called_By_Requests is
     new LSP.Generic_Requests (TextDocumentPositionParams);

   type ALS_Called_By_Request is
     new ALS_Called_By_Requests.Request with null record;

end LSP.Messages.Server_Requests;
