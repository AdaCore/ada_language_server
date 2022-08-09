------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2020-2022, AdaCore                     --
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

with Ada.Strings.UTF_Encoding;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;

with Laltools.Common;

with LSP.Common;
with LSP.Messages;
with LSP.Messages.Client_Requests;
with LSP.Lal_Utils;

with VSS.Strings.Conversions;

package body LSP.Ada_Handlers.Refactor_Imports_Commands is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self         : in out Command'Class;
      Context      : LSP.Ada_Contexts.Context;
      Where        : LSP.Messages.TextDocumentPositionParams;
      With_Clause  : VSS.Strings.Virtual_String;
      Prefix       : VSS.Strings.Virtual_String) is
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
                  LSP.Types.Read_String (JS, V.Context);
               elsif Key = "where" then
                  LSP.Messages.TextDocumentPositionParams'Read (JS, V.Where);
               elsif Key = "with_clause" then
                  LSP.Types.Read_String (JS, V.With_Clause);
               elsif Key = "prefix" then
                  LSP.Types.Read_String (JS, V.Prefix);
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
         return VSS.Strings.Virtual_String;
      --  Creates the suggestion text that will be shown by the client to
      --  to the developer. The text is costumized based on the need of
      --  and with clause and/or prefix.

      ------------------------------
      -- Create_Suggestions_Title --
      ------------------------------
      function Create_Suggestion_Title
        (Suggestion : Laltools.Refactor_Imports.Import_Suggestion)
         return VSS.Strings.Virtual_String
      is
         Title : Ada.Strings.Wide_Wide_Unbounded.
           Unbounded_Wide_Wide_String
             := Ada.Strings.Wide_Wide_Unbounded.
               Null_Unbounded_Wide_Wide_String;
         use type Ada.Strings.Wide_Wide_Unbounded.
           Unbounded_Wide_Wide_String;

      begin
         if Suggestion.With_Clause_Text /= "" then
            if Suggestion.Prefix_Text /= "" then
               --  Add with clause and prefix
               Title :=
                 Title
                 & "Add 'with' clause for "
                 & Suggestion.With_Clause_Text
                 & " and prefix the object with "
                 & Suggestion.Prefix_Text;

            else
               --  Add with clause and leave the prefix as it is
               Title :=
                 Title
                 & "Add 'with' clause for "
                 & Suggestion.With_Clause_Text;
            end if;
         else
            --  Only add prefix

            Title := Title & "Prefix the object with "
              & Suggestion.Prefix_Text;
         end if;
         return VSS.Strings.To_Virtual_String
           (Langkit_Support.Text.To_Text (Title));
      end Create_Suggestion_Title;

   begin
      Self.Initialize
        (Context     => Context.all,
         Where       => ((uri => Where.uri),
                         Where.span.first),
         With_Clause =>
           VSS.Strings.Conversions.To_Virtual_String
             (Suggestion.With_Clause_Text),
         Prefix      =>
           VSS.Strings.Conversions.To_Virtual_String
             (Suggestion.Prefix_Text));
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
                            title      => <>,
                            Custom     => Pointer)));
      Commands_Vector.Append (Item);
   end Append_Suggestion;

   ----------------------------------
   -- Command_To_Refactoring_Edits --
   ----------------------------------

   function Command_To_Refactoring_Edits
     (Self     : Command;
      Context  : LSP.Ada_Contexts.Context;
      Document : LSP.Ada_Documents.Document_Access)
      return Laltools.Refactor.Refactoring_Edits
   is
      use Langkit_Support.Text;
      use Libadalang.Analysis;
      use Libadalang.Common;
      use Libadalang.Slocs;
      use Laltools.Refactor;
      use VSS.Strings;
      use VSS.Strings.Conversions;

      Node : Ada_Node :=
        Document.Get_Node_At (Context, Self.Where.position);

      Edits :  Laltools.Refactor.Refactoring_Edits;

   begin
      --  Add prefix

      if not Self.Prefix.Is_Empty
        and then Node.Kind in Ada_Identifier
      then
         --  If this is a DottedName them remove the current prefix and replace
         --  it by the suggested one. Otherwise, just add the prepend the
         --  prefix

         while Node.Parent.Kind in Ada_Dotted_Name_Range loop
            Node := Node.Parent;
         end loop;

         if Node.Kind in Ada_Dotted_Name_Range then
            Node := Node.As_Dotted_Name.F_Suffix.As_Ada_Node;
         end if;

         if Node.Parent.Kind = Ada_Dotted_Name then
            --  Node.Parent is the full Dotted Name: this includes the
            --  current prefixes and the identifier. Using this SLOC instead
            --  of only the current prefixes SLOC is better since this covers
            --  cases when the Dotted Name is splitted in multiple lines.

            Safe_Insert
              (Edits     => Edits.Text_Edits,
               File_Name => Node.Unit.Get_Filename,
               Edit      =>
                 Text_Edit'
                   (Location =>
                      Make_Range
                        (Start_Sloc
                           (Node.Parent.As_Dotted_Name.F_Prefix.Sloc_Range),
                         Start_Sloc (Node.Sloc_Range)),
                    Text     =>
                      Ada.Strings.Unbounded.To_Unbounded_String
                        (To_UTF8 (To_Wide_Wide_String (Self.Prefix)))));

         else
            Safe_Insert
              (Edits     => Edits.Text_Edits,
               File_Name => Node.Unit.Get_Filename,
               Edit      =>
                 Text_Edit'
                   (Location =>
                      Make_Range
                        (Start_Sloc (Node.Sloc_Range),
                         Start_Sloc (Node.Sloc_Range)),
                    Text     =>
                      Ada.Strings.Unbounded.To_Unbounded_String
                        (To_UTF8 (To_Wide_Wide_String (Self.Prefix)))));
         end if;
      end if;

      --  Add with clause

      if not Self.With_Clause.Is_Empty then
         declare
            Last : Boolean;
            S    : constant Libadalang.Slocs.Source_Location :=
              Laltools.Common.Get_Insert_With_Location
                (Node      => Laltools.Common.Get_Compilation_Unit (Node),
                 Pack_Name =>
                   VSS.Strings.Conversions.To_Wide_Wide_String
                     (Self.With_Clause),
                 Last      => Last);
         begin
            if S /= Libadalang.Slocs.No_Source_Location then
               if Last then
                  Safe_Insert
                    (Edits     => Edits.Text_Edits,
                     File_Name => Node.Unit.Get_Filename,
                     Edit      =>
                       Text_Edit'
                         (Location => Make_Range (S, S),
                          Text     =>
                            Ada.Strings.Unbounded.To_Unbounded_String
                              (To_UTF8 (To_Wide_Wide_String
                               (Document.Line_Terminator
                               & "with " & Self.With_Clause & ";")))));

               else
                  Safe_Insert
                    (Edits     => Edits.Text_Edits,
                     File_Name => Node.Unit.Get_Filename,
                     Edit      =>
                       Text_Edit'
                         (Location => Make_Range (S, S),
                          Text     =>
                            Ada.Strings.Unbounded.To_Unbounded_String
                              (To_UTF8 (To_Wide_Wide_String
                               ("with " & Self.With_Clause & ";"
                                  & Document.Line_Terminator)))));
               end if;

            end if;
         end;
      end if;

      return Edits;
   end Command_To_Refactoring_Edits;

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
      use Laltools.Refactor;
      use LSP.Messages;
      use LSP.Types;
      use VSS.Strings;
      use VSS.Strings.Conversions;

      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context).all;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Message_Handler.Get_Open_Document (Self.Where.textDocument.uri);

      Apply           : Client_Requests.Workspace_Apply_Edit_Request;
      Workspace_Edits : WorkspaceEdit renames Apply.params.edit;
      Label           : Optional_Virtual_String renames Apply.params.label;

      Edits : constant Refactoring_Edits :=
        Self.Command_To_Refactoring_Edits (Context, Document);

   begin
      Workspace_Edits :=
        LSP.Lal_Utils.To_Workspace_Edit
          (Edits               => Edits,
           Resource_Operations => Message_Handler.Resource_Operations,
           Versioned_Documents => Message_Handler.Versioned_Documents,
           Document_Provider   => Message_Handler'Access);
      Label :=
        (Is_Set => True,
         Value  => To_Virtual_String (Command'External_Tag));

      Client.On_Workspace_Apply_Edit_Request (Apply);

   exception
      when E : others =>
         LSP.Common.Log (Message_Handler.Trace, E);
         Error :=
           (Is_Set => True,
            Value  =>
              (code    => LSP.Errors.UnknownErrorCode,
               message => VSS.Strings.Conversions.To_Virtual_String
                 ("Failed to execute the Auto Imports refactoring"),
               data    => <>));
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
      LSP.Types.Write_String (S, V.Context);
      JS.Key ("where");
      LSP.Messages.TextDocumentPositionParams'Write (S, V.Where);
      JS.Key ("with_clause");
      LSP.Types.Write_String (S, V.With_Clause);
      JS.Key ("prefix");
      LSP.Types.Write_String (S, V.Prefix);
      JS.End_Object;
   end Write_Command;

end LSP.Ada_Handlers.Refactor_Imports_Commands;
