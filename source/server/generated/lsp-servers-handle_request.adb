--  Automatically generated, do not edit.

with LSP.Messages.Server_Requests; use LSP.Messages.Server_Requests;

function LSP.Servers.Handle_Request
  (Self    : not null Server_Request_Handlers
     .Server_Request_Handler_Access;
   Request : LSP.Messages.RequestMessage'Class)
      return LSP.Messages.ResponseMessage'Class is
begin

      if Request in Initialize_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Initialize_Request
                  (Initialize_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Shutdown_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Shutdown_Request
                  (Shutdown_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in CodeAction_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_CodeAction_Request
                  (CodeAction_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Completion_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Completion_Request
                  (Completion_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in CompletionItemResolve_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_CompletionItemResolve_Request
                  (CompletionItemResolve_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Definition_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Definition_Request
                  (Definition_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Declaration_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Declaration_Request
                  (Declaration_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Implementation_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Implementation_Request
                  (Implementation_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Type_Definition_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Type_Definition_Request
                  (Type_Definition_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Highlight_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Highlight_Request
                  (Highlight_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Hover_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Hover_Request
                  (Hover_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Document_Links_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Document_Links_Request
                  (Document_Links_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in References_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_References_Request
                  (References_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Signature_Help_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Signature_Help_Request
                  (Signature_Help_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Document_Symbols_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Document_Symbols_Request
                  (Document_Symbols_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Rename_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Rename_Request
                  (Rename_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Prepare_Rename_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Prepare_Rename_Request
                  (Prepare_Rename_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Execute_Command_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Execute_Command_Request
                  (Execute_Command_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Document_Color_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Document_Color_Request
                  (Document_Color_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Color_Presentation_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Color_Presentation_Request
                  (Color_Presentation_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Folding_Range_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Folding_Range_Request
                  (Folding_Range_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Formatting_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Formatting_Request
                  (Formatting_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Range_Formatting_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Range_Formatting_Request
                  (Range_Formatting_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Selection_Range_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Selection_Range_Request
                  (Selection_Range_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Document_Tokens_Full_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Document_Tokens_Full_Request
                  (Document_Tokens_Full_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Document_Tokens_Range_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Document_Tokens_Range_Request
                  (Document_Tokens_Range_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Prepare_Call_Hierarchy_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Prepare_Call_Hierarchy_Request
                  (Prepare_Call_Hierarchy_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Incoming_Calls_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Incoming_Calls_Request
                  (Incoming_Calls_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Outgoing_Calls_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Outgoing_Calls_Request
                  (Outgoing_Calls_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Workspace_Symbols_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Workspace_Symbols_Request
                  (Workspace_Symbols_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Workspace_Execute_Command_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Workspace_Execute_Command_Request
                  (Workspace_Execute_Command_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Workspace_Will_Create_Files_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Workspace_Will_Create_Files_Request
                  (Workspace_Will_Create_Files_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Workspace_Will_Rename_Files_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Workspace_Will_Rename_Files_Request
                  (Workspace_Will_Rename_Files_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Workspace_Will_Delete_Files_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Workspace_Will_Delete_Files_Request
                  (Workspace_Will_Delete_Files_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in ALS_Show_Dependencies_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_ALS_Show_Dependencies_Request
                  (ALS_Show_Dependencies_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in ALS_Source_Dirs_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_ALS_Source_Dirs_Request
                  (ALS_Source_Dirs_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in ALS_Debug_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_ALS_Debug_Request
                  (ALS_Debug_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in ALS_Check_Syntax_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_ALS_Check_Syntax_Request
                  (ALS_Check_Syntax_Request (Request));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

   return LSP.Messages.ResponseMessage'
     (Is_Error => True,
      jsonrpc  => <>,
      id       => <>,
      error    =>
        (Is_Set => True,
         Value  =>
           (code    => LSP.Messages.MethodNotFound,
            message => "The Request handler doesn't support this",
            others  => <>)));
end LSP.Servers.Handle_Request;
