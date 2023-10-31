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

with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Text;

with Libadalang.Analysis;

with VSS.JSON.Streams;
with VSS.Strings.Conversions;

with LSP.Enumerations;
with LSP.Structures.LSPAny_Vectors; use LSP.Structures.LSPAny_Vectors;

package body LSP.Ada_Handlers.Refactor.Auto_Import is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self         : in out Command'Class;
      Context      : LSP.Ada_Contexts.Context;
      Where        : LSP.Structures.TextDocumentPositionParams;
      With_Clause  : VSS.Strings.Virtual_String;
      Prefix       : VSS.Strings.Virtual_String) is
   begin
      Self.Context     := Context.Id;
      Self.Where       := Where;
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
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Command
   is
      use VSS.JSON.Streams;
      use VSS.Strings;
      use LSP.Structures.JSON_Event_Vectors;

      C : Cursor := Any.First;
   begin
      return Self : Command do
         pragma Assert (Element (C).Kind = Start_Array);
         Next (C);
         pragma Assert (Element (C).Kind = Start_Object);
         Next (C);

         while Has_Element (C)
           and then Element (C).Kind /= End_Object
         loop
            pragma Assert (Element (C).Kind = Key_Name);
            declare
               Key : constant Virtual_String := Element (C).Key_Name;
            begin
               Next (C);

               if Key = "context" then
                  Self.Context := Element (C).String_Value;

               elsif Key = "where" then
                  Self.Where := From_Any (C);

               elsif Key = "import" then
                  Self.Suggestion.Import :=
                    VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String
                      (From_Any (C));

               elsif Key = "qualifier" then
                  Self.Suggestion.Qualifier :=
                    VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String
                      (From_Any (C));

               else
                  Skip_Value (C);
               end if;
            end;

            Next (C);
         end loop;
      end return;
   end Create;

   -------------------------------------
   -- Append_Refactor_Imports_Command --
   -------------------------------------

   procedure Append_Suggestion
     (Self            : in out Command;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Where           : LSP.Structures.Location;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Suggestion      : LAL_Refactor.Auto_Import.Import_Type)
   is
      Item    : LSP.Structures.CodeAction;

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
                         Where.a_range.start),
         With_Clause =>
           VSS.Strings.Conversions.To_Virtual_String
             (Suggestion.Import),
         Prefix      =>
           VSS.Strings.Conversions.To_Virtual_String
             (Suggestion.Qualifier));

      Item :=
        (title       => Create_Suggestion_Title (Suggestion),
         kind        => (Is_Set => True,
                         Value  => LSP.Enumerations.QuickFix),
         diagnostics => <>,
         disabled    => (Is_Set => False),
         edit        => (Is_Set => False),
         isPreferred => (Is_Set => False),
         command     => (Is_Set => True,
                         Value  =>
                           (title     => <>,
                            command   => VSS.Strings.Conversions.
                              To_Virtual_String (Command'External_Tag),
                            arguments => Self.Write_Command)),
         data        => <>);

      Commands_Vector.Append
        (LSP.Structures.Command_Or_CodeAction'
           (Is_Command => False, CodeAction => Item));
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
      Handler : not null access LSP.Ada_Handlers.Message_Handler'Class;
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

   function Write_Command
     (Self : Command) return LSP.Structures.LSPAny_Vector
   is
      use VSS.JSON.Streams;

      Result : LSP.Structures.LSPAny_Vector;

   begin
      Result.Append (JSON_Stream_Element'(Kind => Start_Array));
      Result.Append (JSON_Stream_Element'(Kind => Start_Object));

      --  "context"
      Add_Key ("context", Result);
      To_Any (Self.Context, Result);

      --  "where"
      Add_Key ("where", Result);
      To_Any (Self.Where, Result);

      --  "import"
      Add_Key ("import", Result);
      To_Any
        (VSS.Strings.Conversions.To_Virtual_String (Self.Suggestion.Import),
         Result);

      --  "qualifier"
      Add_Key ("qualifier", Result);
      To_Any
        (VSS.Strings.Conversions.To_Virtual_String (Self.Suggestion.Qualifier),
         Result);

      Result.Append (JSON_Stream_Element'(Kind => End_Object));
      Result.Append (JSON_Stream_Element'(Kind => End_Array));

      return Result;
   end Write_Command;

end LSP.Ada_Handlers.Refactor.Auto_Import;
