--  Automatically generated, do not edit.

with Ada.Tags;
with LSP.Generic_Requests;
with LSP.JSON_Streams;
with LSP.Server_Request_Receivers;
use LSP.Server_Request_Receivers;

package LSP.Messages.Server_Requests is

   type Server_Request is abstract new LSP.Messages.RequestMessage with record
      Canceled : Boolean := False with Atomic;
   end record;

   function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
      return Server_Request is abstract;

   procedure Visit
     (Self    : Server_Request;
      Handler : access Server_Request_Receiver'Class) is abstract;

   function Method_To_Tag (Method : LSP.Types.LSP_String) return Ada.Tags.Tag;
   --  For given LSP method return a corresponding message type tag

   package Initialize_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        InitializeParams,
        Server_Request_Receiver'Class);

   type Initialize_Request is
     new Initialize_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Initialize_Request;
      Handler : access Server_Request_Receiver'Class);

   type Shutdown_Request is new Server_Request with null record;

   overriding function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
      return Shutdown_Request;

   overriding procedure Visit
     (Self    : Shutdown_Request;
      Handler : access Server_Request_Receiver'Class);

   package CodeAction_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        CodeActionParams,
        Server_Request_Receiver'Class);

   type CodeAction_Request is
     new CodeAction_Requests.Request with null record;

   overriding procedure Visit
     (Self    : CodeAction_Request;
      Handler : access Server_Request_Receiver'Class);

   package Completion_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        TextDocumentPositionParams,
        Server_Request_Receiver'Class);

   type Completion_Request is
     new Completion_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Completion_Request;
      Handler : access Server_Request_Receiver'Class);

   package Definition_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        TextDocumentPositionParams,
        Server_Request_Receiver'Class);

   type Definition_Request is
     new Definition_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Definition_Request;
      Handler : access Server_Request_Receiver'Class);

   package Declaration_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        TextDocumentPositionParams,
        Server_Request_Receiver'Class);

   type Declaration_Request is
     new Declaration_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Declaration_Request;
      Handler : access Server_Request_Receiver'Class);

   package Implementation_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        TextDocumentPositionParams,
        Server_Request_Receiver'Class);

   type Implementation_Request is
     new Implementation_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Implementation_Request;
      Handler : access Server_Request_Receiver'Class);

   package Type_Definition_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        TextDocumentPositionParams,
        Server_Request_Receiver'Class);

   type Type_Definition_Request is
     new Type_Definition_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Type_Definition_Request;
      Handler : access Server_Request_Receiver'Class);

   package Highlight_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        TextDocumentPositionParams,
        Server_Request_Receiver'Class);

   type Highlight_Request is
     new Highlight_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Highlight_Request;
      Handler : access Server_Request_Receiver'Class);

   package Hover_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        TextDocumentPositionParams,
        Server_Request_Receiver'Class);

   type Hover_Request is
     new Hover_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Hover_Request;
      Handler : access Server_Request_Receiver'Class);

   package References_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        ReferenceParams,
        Server_Request_Receiver'Class);

   type References_Request is
     new References_Requests.Request with null record;

   overriding procedure Visit
     (Self    : References_Request;
      Handler : access Server_Request_Receiver'Class);

   package Signature_Help_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        TextDocumentPositionParams,
        Server_Request_Receiver'Class);

   type Signature_Help_Request is
     new Signature_Help_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Signature_Help_Request;
      Handler : access Server_Request_Receiver'Class);

   package Document_Symbols_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        DocumentSymbolParams,
        Server_Request_Receiver'Class);

   type Document_Symbols_Request is
     new Document_Symbols_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Document_Symbols_Request;
      Handler : access Server_Request_Receiver'Class);

   package Rename_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        RenameParams,
        Server_Request_Receiver'Class);

   type Rename_Request is
     new Rename_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Rename_Request;
      Handler : access Server_Request_Receiver'Class);

   package Execute_Command_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        ExecuteCommandParams,
        Server_Request_Receiver'Class);

   type Execute_Command_Request is
     new Execute_Command_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Execute_Command_Request;
      Handler : access Server_Request_Receiver'Class);

   package Workspace_Symbols_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        WorkspaceSymbolParams,
        Server_Request_Receiver'Class);

   type Workspace_Symbols_Request is
     new Workspace_Symbols_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Workspace_Symbols_Request;
      Handler : access Server_Request_Receiver'Class);

   package Workspace_Execute_Command_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        ExecuteCommandParams,
        Server_Request_Receiver'Class);

   type Workspace_Execute_Command_Request is
     new Workspace_Execute_Command_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Workspace_Execute_Command_Request;
      Handler : access Server_Request_Receiver'Class);

   package ALS_Called_By_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        TextDocumentPositionParams,
        Server_Request_Receiver'Class);

   type ALS_Called_By_Request is
     new ALS_Called_By_Requests.Request with null record;

   overriding procedure Visit
     (Self    : ALS_Called_By_Request;
      Handler : access Server_Request_Receiver'Class);

   package ALS_Debug_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        ALSDebugParams,
        Server_Request_Receiver'Class);

   type ALS_Debug_Request is
     new ALS_Debug_Requests.Request with null record;

   overriding procedure Visit
     (Self    : ALS_Debug_Request;
      Handler : access Server_Request_Receiver'Class);

end LSP.Messages.Server_Requests;
