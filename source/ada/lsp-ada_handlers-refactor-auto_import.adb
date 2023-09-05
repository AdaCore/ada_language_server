------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Text;

with Libadalang.Analysis;

with LSP.Commands;
with LSP.Messages;

with VSS.Strings.Conversions;

package body LSP.Ada_Handlers.Refactor.Auto_Import is

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
      Self.Suggestion :=
        (Import    =>
           VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String (With_Clause),
         Qualifier =>
           VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String (Prefix));
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
               elsif Key = "import" then
                  declare
                     Import : VSS.Strings.Virtual_String;
                  begin
                     LSP.Types.Read_String (JS, Import);
                     V.Suggestion.Import :=
                       VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String
                         (Import);
                  end;

               elsif Key = "qualifier" then
                  declare
                     Qualififer : VSS.Strings.Virtual_String;
                  begin
                     LSP.Types.Read_String (JS, Qualififer);
                     V.Suggestion.Qualifier :=
                       VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String
                         (Qualififer);
                  end;
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
      Suggestion        : LAL_Refactor.Auto_Import.Import_Type)
   is
      Pointer : LSP.Commands.Command_Pointer;
      Item    : LSP.Messages.CodeAction;

      function Create_Suggestion_Title
        (Suggestion : LAL_Refactor.Auto_Import.Import_Type)
         return VSS.Strings.Virtual_String;
      --  Creates the suggestion text that will be shown by the client to
      --  to the developer. The text is costumized based on the need of
      --  and with clause and/or prefix.

      ------------------------------
      -- Create_Suggestions_Title --
      ------------------------------
      function Create_Suggestion_Title
        (Suggestion : LAL_Refactor.Auto_Import.Import_Type)
         return VSS.Strings.Virtual_String
      is
         use Ada.Strings.Wide_Wide_Unbounded;

         Title : constant Langkit_Support.Text.Unbounded_Text_Type :=
           "Qualify with " & Suggestion.Qualifier;
      begin
         return
           VSS.Strings.To_Virtual_String
             (Langkit_Support.Text.To_Text (Title));
      end Create_Suggestion_Title;

   begin
      Self.Initialize
        (Context     => Context.all,
         Where       => ((uri => Where.uri),
                         Where.span.first),
         With_Clause =>
           VSS.Strings.Conversions.To_Virtual_String
             (Suggestion.Import),
         Prefix      =>
           VSS.Strings.Conversions.To_Virtual_String
             (Suggestion.Qualifier));
      Pointer.Set (Self);
      Item :=
        (title       => Create_Suggestion_Title (Suggestion),
         kind        => (Is_Set => True,
                         Value  => LSP.Messages.QuickFix),
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
      return LAL_Refactor.Refactoring_Edits
   is
      use Libadalang.Analysis;
      use LAL_Refactor.Auto_Import;

      Name : constant Libadalang.Analysis.Name :=
        Document.Get_Node_At (Context, Self.Where.position).As_Name;

      function Units return Analysis_Unit_Array is ([]);

   begin
      return
        Create_Auto_Importer
          (Name,
           Self.Suggestion)
          .Refactor (Units'Access);
   end Command_To_Refactoring_Edits;

   --------------
   -- Refactor --
   --------------

   overriding procedure Refactor
     (Self    : Command;
      Handler : not null access LSP.Server_Notification_Receivers.
        Server_Notification_Receiver'Class;
      Client : not null access LSP.Client_Message_Receivers.
        Client_Message_Receiver'Class;
      Edits   : out LAL_Refactor.Refactoring_Edits)
   is
      use LAL_Refactor;

      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context).all;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Message_Handler.Get_Open_Document (Self.Where.textDocument.uri);

   begin
      Edits := Self.Command_To_Refactoring_Edits (Context, Document);
   end Refactor;

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
      JS.Key ("import");
      LSP.Types.Write_String
        (S,
         VSS.Strings.Conversions.To_Virtual_String (V.Suggestion.Import));
      JS.Key ("qualifier");
      LSP.Types.Write_String
        (S,
         VSS.Strings.Conversions.To_Virtual_String (V.Suggestion.Qualifier));
      JS.End_Object;
   end Write_Command;

end LSP.Ada_Handlers.Refactor.Auto_Import;
