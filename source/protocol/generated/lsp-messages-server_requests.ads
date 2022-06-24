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

   function Method_To_Tag
     (Method : VSS.Strings.Virtual_String) return Ada.Tags.Tag;
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

   package CompletionItemResolve_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        CompletionItem,
        Server_Request_Receiver'Class);

   type CompletionItemResolve_Request is
     new CompletionItemResolve_Requests.Request with null record;

   overriding procedure Visit
     (Self    : CompletionItemResolve_Request;
      Handler : access Server_Request_Receiver'Class);

   package Definition_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        DefinitionParams,
        Server_Request_Receiver'Class);

   type Definition_Request is
     new Definition_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Definition_Request;
      Handler : access Server_Request_Receiver'Class);

   package Declaration_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        DeclarationParams,
        Server_Request_Receiver'Class);

   type Declaration_Request is
     new Declaration_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Declaration_Request;
      Handler : access Server_Request_Receiver'Class);

   package Implementation_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        ImplementationParams,
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

   package Document_Links_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        DocumentLinkParams,
        Server_Request_Receiver'Class);

   type Document_Links_Request is
     new Document_Links_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Document_Links_Request;
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
        SignatureHelpParams,
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

   package Prepare_Rename_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        PrepareRenameParams,
        Server_Request_Receiver'Class);

   type Prepare_Rename_Request is
     new Prepare_Rename_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Prepare_Rename_Request;
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

   package Document_Color_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        DocumentColorParams,
        Server_Request_Receiver'Class);

   type Document_Color_Request is
     new Document_Color_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Document_Color_Request;
      Handler : access Server_Request_Receiver'Class);

   package Color_Presentation_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        ColorPresentationParams,
        Server_Request_Receiver'Class);

   type Color_Presentation_Request is
     new Color_Presentation_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Color_Presentation_Request;
      Handler : access Server_Request_Receiver'Class);

   package Folding_Range_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        FoldingRangeParams,
        Server_Request_Receiver'Class);

   type Folding_Range_Request is
     new Folding_Range_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Folding_Range_Request;
      Handler : access Server_Request_Receiver'Class);

   package Formatting_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        DocumentFormattingParams,
        Server_Request_Receiver'Class);

   type Formatting_Request is
     new Formatting_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Formatting_Request;
      Handler : access Server_Request_Receiver'Class);

   package Range_Formatting_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        DocumentRangeFormattingParams,
        Server_Request_Receiver'Class);

   type Range_Formatting_Request is
     new Range_Formatting_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Range_Formatting_Request;
      Handler : access Server_Request_Receiver'Class);

   package Selection_Range_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        SelectionRangeParams,
        Server_Request_Receiver'Class);

   type Selection_Range_Request is
     new Selection_Range_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Selection_Range_Request;
      Handler : access Server_Request_Receiver'Class);

   package Document_Tokens_Full_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        SemanticTokensParams,
        Server_Request_Receiver'Class);

   type Document_Tokens_Full_Request is
     new Document_Tokens_Full_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Document_Tokens_Full_Request;
      Handler : access Server_Request_Receiver'Class);

   package Document_Tokens_Range_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        SemanticTokensRangeParams,
        Server_Request_Receiver'Class);

   type Document_Tokens_Range_Request is
     new Document_Tokens_Range_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Document_Tokens_Range_Request;
      Handler : access Server_Request_Receiver'Class);

   package Prepare_Call_Hierarchy_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        CallHierarchyPrepareParams,
        Server_Request_Receiver'Class);

   type Prepare_Call_Hierarchy_Request is
     new Prepare_Call_Hierarchy_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Prepare_Call_Hierarchy_Request;
      Handler : access Server_Request_Receiver'Class);

   package Incoming_Calls_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        CallHierarchyIncomingCallsParams,
        Server_Request_Receiver'Class);

   type Incoming_Calls_Request is
     new Incoming_Calls_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Incoming_Calls_Request;
      Handler : access Server_Request_Receiver'Class);

   package Outgoing_Calls_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        CallHierarchyOutgoingCallsParams,
        Server_Request_Receiver'Class);

   type Outgoing_Calls_Request is
     new Outgoing_Calls_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Outgoing_Calls_Request;
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

   package Workspace_Will_Create_Files_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        CreateFilesParams,
        Server_Request_Receiver'Class);

   type Workspace_Will_Create_Files_Request is
     new Workspace_Will_Create_Files_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Workspace_Will_Create_Files_Request;
      Handler : access Server_Request_Receiver'Class);

   package Workspace_Will_Rename_Files_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        RenameFilesParams,
        Server_Request_Receiver'Class);

   type Workspace_Will_Rename_Files_Request is
     new Workspace_Will_Rename_Files_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Workspace_Will_Rename_Files_Request;
      Handler : access Server_Request_Receiver'Class);

   package Workspace_Will_Delete_Files_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        DeleteFilesParams,
        Server_Request_Receiver'Class);

   type Workspace_Will_Delete_Files_Request is
     new Workspace_Will_Delete_Files_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Workspace_Will_Delete_Files_Request;
      Handler : access Server_Request_Receiver'Class);

   package ALS_Show_Dependencies_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        ALS_ShowDependenciesParams,
        Server_Request_Receiver'Class);

   type ALS_Show_Dependencies_Request is
     new ALS_Show_Dependencies_Requests.Request with null record;

   overriding procedure Visit
     (Self    : ALS_Show_Dependencies_Request;
      Handler : access Server_Request_Receiver'Class);

   type ALS_Source_Dirs_Request is new Server_Request with null record;

   overriding function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
      return ALS_Source_Dirs_Request;

   overriding procedure Visit
     (Self    : ALS_Source_Dirs_Request;
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

   package ALS_Check_Syntax_Requests is
     new LSP.Generic_Requests
       (Server_Request,
        ALS_Check_Syntax_Params,
        Server_Request_Receiver'Class);

   type ALS_Check_Syntax_Request is
     new ALS_Check_Syntax_Requests.Request with null record;

   overriding procedure Visit
     (Self    : ALS_Check_Syntax_Request;
      Handler : access Server_Request_Receiver'Class);

end LSP.Messages.Server_Requests;
