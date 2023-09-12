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

with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;

with Laltools.Common;

with VSS.JSON.Streams;

with LSP.Enumerations;
with LSP.Structures.LSPAny_Vectors; use LSP.Structures.LSPAny_Vectors;

package body LSP.Ada_Handlers.Refactor.Imports_Commands is

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
      Self.With_Clause := With_Clause;
      Self.Prefix      := Prefix;
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

               elsif Key = "with_clause" then
                  Self.With_Clause := Element (C).String_Value;

               elsif Key = "prefix" then
                  Self.Prefix := Element (C).String_Value;

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
     (Self              : in out Command;
      Context           : LSP.Ada_Context_Sets.Context_Access;
      Where             : LSP.Structures.Location;
      Commands_Vector   : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Suggestion        : LAL_Refactor.Refactor_Imports.Import_Suggestion)
   is
      Item    : LSP.Structures.CodeAction;

      function Create_Suggestion_Title
        (Suggestion : LAL_Refactor.Refactor_Imports.Import_Suggestion)
         return VSS.Strings.Virtual_String;
      --  Creates the suggestion text that will be shown by the client to
      --  to the developer. The text is costumized based on the need of
      --  and with clause and/or prefix.

      ------------------------------
      -- Create_Suggestions_Title --
      ------------------------------

      function Create_Suggestion_Title
        (Suggestion : LAL_Refactor.Refactor_Imports.Import_Suggestion)
         return VSS.Strings.Virtual_String
      is
         Title : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String :=
             Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;
         use type Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

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
                         Where.a_range.start),
         With_Clause =>
           VSS.Strings.Conversions.To_Virtual_String
             (Suggestion.With_Clause_Text),
         Prefix      =>
           VSS.Strings.Conversions.To_Virtual_String
             (Suggestion.Prefix_Text));

      Item :=
        (title       => Create_Suggestion_Title (Suggestion),
         kind        => (Is_Set => True,
                         Value  => LSP.Enumerations.RefactorRewrite),
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
      use Langkit_Support.Text;
      use Libadalang.Analysis;
      use Libadalang.Common;
      use Libadalang.Slocs;
      use LAL_Refactor;
      use VSS.Strings;
      use VSS.Strings.Conversions;

      Node : Ada_Node :=
        Document.Get_Node_At (Context, Self.Where.position);

      Edits :  LAL_Refactor.Refactoring_Edits;

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

      --  "with_clause"
      Add_Key ("with_clause", Result);
      To_Any (Self.With_Clause, Result);

      --  "prefix"
      Add_Key ("prefix", Result);
      To_Any (Self.Prefix, Result);

      Result.Append (JSON_Stream_Element'(Kind => End_Object));
      Result.Append (JSON_Stream_Element'(Kind => End_Array));

      return Result;
   end Write_Command;

end LSP.Ada_Handlers.Refactor.Imports_Commands;
