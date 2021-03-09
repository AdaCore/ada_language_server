------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
with Ada.Exceptions;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Wide_Unbounded;

with Libadalang.Analysis;
with Libadalang.Common;

with Langkit_Support.Text;

with LSP.Messages;
with LSP.Messages.Client_Requests;
with LSP.Lal_Utils;

with VSS.Strings;
with VSS.Strings.Conversions;

package body LSP.Ada_Handlers.Refactor_Imports_Commands is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Command'Class;
      Context : LSP.Ada_Contexts.Context;
      Where   : LSP.Messages.TextDocumentPositionParams;
      With_Clause  : LSP.Types.LSP_String;
      Prefix  : LSP.Types.LSP_String) is
   begin
      Self.Context := Context.Id;
      Self.Where := Where;
      Self.With_Clause := With_Clause;
      Self.Prefix := Prefix;
   end Initialize;

   ------------
   -- Create --
   ------------

   overriding function Create
     (JS : not null access LSP.JSON_Streams.JSON_Stream'Class)
      return Command
   is
   begin
      return V : Command do
         pragma Assert (JS.R.Is_Start_Object);
         JS.R.Read_Next;
         while not JS.R.Is_End_Object loop
            pragma Assert (JS.R.Is_Key_Name);
            declare
               Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
                 VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
            begin
               JS.R.Read_Next;

               if Key = "context" then
                  LSP.Types.Read (JS, V.Context);
               elsif Key = "where" then
                  LSP.Messages.TextDocumentPositionParams'Read (JS, V.Where);
               elsif Key = "with_clause" then
                  LSP.Types.Read (JS, V.With_Clause);
               elsif Key = "prefix" then
                  LSP.Types.Read (JS, V.Prefix);
               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end return;
   end Create;

   -------------------------------------
   -- Append_Refactor_Imports_Command --
   -------------------------------------

   procedure Append_Suggestion
     (Self              : in out Command;
      Context           : Context_Access;
      Where             : LSP.Messages.Location;
      Commands_Vector   : in out LSP.Messages.CodeAction_Vector;
      Suggestion        : Laltools.Refactor_Imports.Import_Suggestion)
   is
      Pointer : LSP.Commands.Command_Pointer;
      Item    : LSP.Messages.CodeAction;

      function Create_Suggestion_Title
        (Suggestion : Laltools.Refactor_Imports.Import_Suggestion)
               return LSP.Types.LSP_String;
      --  Creates the suggestion text that will be shown by the client to
      --  to the developer. The text is costumized based on the need of
      --  and with clause and/or prefix.

      ------------------------------
      -- Create_Suggestions_Title --
      ------------------------------
      function Create_Suggestion_Title
        (Suggestion : Laltools.Refactor_Imports.Import_Suggestion)
         return LSP.Types.LSP_String
      is
         Title : Ada.Strings.Wide_Wide_Unbounded.
           Unbounded_Wide_Wide_String
             := Ada.Strings.Wide_Wide_Unbounded.
               Null_Unbounded_Wide_Wide_String;
         use type Ada.Strings.Wide_Wide_Unbounded.
           Unbounded_Wide_Wide_String;
      begin
         if Suggestion.With_Clause_Text /= "" then
            --  Add with clause and prefix

            Title := Title & "Add 'with' clause for "
              & Suggestion.With_Clause_Text & " and prefix the object with "
              & Suggestion.Prefix_Text;
         else
            --  Only add prefix

            Title := Title & "Prefix the object with "
              & Suggestion.Prefix_Text;
         end if;
         return LSP.Types.To_LSP_String
           (Langkit_Support.Text.To_Text (Title));
      end Create_Suggestion_Title;

   begin
      Self.Initialize
        (Context     => Context.all,
         Where       => ((uri => Where.uri),
                         Where.span.first),
         With_Clause => LSP.Types.To_LSP_String
           (Langkit_Support.Text.To_Text (Suggestion.With_Clause_Text)),
         Prefix      => LSP.Types.To_LSP_String
           (Langkit_Support.Text.To_Text (Suggestion.Prefix_Text)));
      Pointer.Set (Self);
      Item :=
        (title       => Create_Suggestion_Title (Suggestion),
         kind        => (Is_Set => True,
                         Value  => LSP.Messages.RefactorRewrite),
         diagnostics => (Is_Set => False),
         disabled    => (Is_Set => False),
         edit        => (Is_Set => False),
         isPreferred => (Is_Set => False),
         command     => (Is_Set => True,
                         Value  =>
                           (Is_Unknown => False,
                            title      => LSP.Types.Empty_LSP_String,
                            Custom     => Pointer)));
      Commands_Vector.Append (Item);
   end Append_Suggestion;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self    : Command;
      Handler : not null access LSP.Server_Notification_Receivers.
        Server_Notification_Receiver'Class;
      Client : not null access LSP.Client_Message_Receivers.
        Client_Message_Receiver'Class;
      Error : in out LSP.Errors.Optional_ResponseError)
   is
      use type Libadalang.Common.Ada_Node_Kind_Type;

      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context).all;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Message_Handler.Get_Open_Document (Self.Where.textDocument.uri);
      Apply    : LSP.Messages.Client_Requests.Workspace_Apply_Edit_Request;
      Node     : constant Libadalang.Analysis.Ada_Node :=
        Document.Get_Node_At (Context, Self.Where.position);
      Loc      : LSP.Messages.Location;
      Edit     : LSP.Messages.AnnotatedTextEdit;

      Client_Supports_documentChanges : constant Boolean := True;

      Edits    : LSP.Messages.WorkspaceEdit renames Apply.params.edit;
      Version  : constant LSP.Messages.VersionedTextDocumentIdentifier :=
        Document.Versioned_Identifier;
   begin
      Edits.documentChanges.Append
        (LSP.Messages.Document_Change'
           (Kind               => LSP.Messages.Text_Document_Edit,
            Text_Document_Edit =>
              (textDocument => (Version.uri, (True, Version.version)),
               edits        => <>)));

      --  Add prefix.

      if Node.Kind = Libadalang.Common.Ada_Identifier then
         --  If this is a DottedName them remove the current prefix and replace
         --  it by the suggested one. Otherwise, just add the prepend the
         --  prefix

         if Node.Parent.Kind = Libadalang.Common.Ada_Dotted_Name then
            --  Node.Parent is the full Dotted Name: this includes the
            --  current prefixes and the identifier. Using this SLOC instead
            --  of only the current prefixes SLOC is better since this covers
            --  cases when the Dotted Name is splitted in multiple lines.

            Loc := LSP.Lal_Utils.Get_Node_Location (Node.Parent);
            Edit.span := (Loc.span.first, Loc.span.last);
            Edit.newText := LSP.Types.To_LSP_String
              (LSP.Types.To_UTF_8_String (Self.Prefix)
               & Langkit_Support.Text.To_UTF8 (Node.Text));
         else
            Loc := LSP.Lal_Utils.Get_Node_Location (Node);
            Edit.span := (Loc.span.first, Loc.span.first);
            Edit.newText := Self.Prefix;
         end if;

         if Client_Supports_documentChanges then
            Edits.documentChanges (1).Text_Document_Edit.edits.Append (Edit);
         else
            Edits.changes (Edits.changes.First).Append
              (LSP.Messages.TextEdit (Edit));
         end if;
      end if;

      --  Add with clause.

      if not LSP.Types.Is_Empty (Self.With_Clause) then
         declare
            Withed_List : constant Libadalang.Analysis.Ada_Node
              := Node.Unit.Root.Children (1);
            Already_Imported : Boolean := False;
            use type LSP.Types.LSP_String;
         begin
            Loc := LSP.Lal_Utils.Get_Node_Location (Withed_List);

            if Node.Unit.Root.Children (1).Children'Length = 0 then
               --  If this compilation unit does not have 'with' clauses, then
               --  simply add the 'with' statement.

               Edit.span := (Loc.span.first,
                             Loc.span.first);
               Edit.newText := LSP.Types.To_LSP_String
                 (LSP.Types.To_UTF_8_String ("with " & Self.With_Clause & ";")
                  & ASCII.LF);
            else
               --  Otherwise, check if it already imports the seggested unit,
               --  and if not, add the 'with' statement.

               Edit.span := (Loc.span.last,
                             Loc.span.last);

               Already_Imported_Loop :
               for With_Clause of Withed_List.Children loop
                  if Self.With_Clause = LSP.Types.To_LSP_String
                    (Langkit_Support.Text.To_UTF8
                       (With_Clause.As_With_Clause.F_Packages.Text))
                  then
                     Already_Imported := True;
                     exit Already_Imported_Loop;
                  end if;
               end loop Already_Imported_Loop;

               if not Already_Imported then

                  Edit.newText := LSP.Types.To_LSP_String
                    (ASCII.LF & LSP.Types.To_UTF_8_String
                       ("with " & Self.With_Clause & ";"));
               else
                  Edit.newText := LSP.Types.Empty_LSP_String;
               end if;
            end if;

            if Client_Supports_documentChanges then
               Edits.documentChanges (1).Text_Document_Edit.edits.Append
                 (Edit);
            else
               Edits.changes (Edits.changes.First).Append
                 (LSP.Messages.TextEdit (Edit));
            end if;
         end;
      end if;

      Client.On_Workspace_Apply_Edit_Request (Apply);
   exception
      when E : others =>
         Error :=
           (Is_Set => True,
            Value  =>
              (code => LSP.Errors.UnknownErrorCode,
               message => LSP.Types.To_LSP_String
                 (Ada.Exceptions.Exception_Information (E)),
               data => <>));
   end Execute;

   -------------------
   -- Write_Command --
   -------------------

   procedure Write_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Command)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("context");
      LSP.Types.Write (S, V.Context);
      JS.Key ("where");
      LSP.Messages.TextDocumentPositionParams'Write (S, V.Where);
      JS.Key ("with_clause");
      LSP.Types.Write (S, V.With_Clause);
      JS.Key ("prefix");
      LSP.Types.Write (S, V.Prefix);
      JS.End_Object;
   end Write_Command;

end LSP.Ada_Handlers.Refactor_Imports_Commands;
