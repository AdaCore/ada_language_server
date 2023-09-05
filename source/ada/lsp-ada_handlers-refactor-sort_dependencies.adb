------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with Langkit_Support.Slocs;

with Libadalang.Analysis;

with LAL_Refactor.Sort_Dependencies;

with VSS.JSON.Streams;
with VSS.Strings.Conversions;

with LSP.Enumerations;
with LSP.Structures.LSPAny_Vectors; use LSP.Structures.LSPAny_Vectors;

package body LSP.Ada_Handlers.Refactor.Sort_Dependencies is

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self            : in out Command;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Where           : LSP.Structures.Location)
   is
      Code_Action : LSP.Structures.CodeAction;

   begin
      Self.Initialize
        (Context           => Context.all,
         Where             => Where);

      Code_Action :=
        (title       => "Sort Dependencies",
         kind        =>
           (Is_Set => True,
            Value  => LSP.Enumerations.Refactor),
         diagnostics => <>,
         edit        => (Is_Set => False),
         isPreferred => (Is_Set => False),
         disabled    => (Is_Set => False),
         command     =>
           (Is_Set => True,
            Value  =>
              (title     => <>,
               command   => VSS.Strings.Conversions.To_Virtual_String
                 (Command'External_Tag),
               arguments => Self.Write_Command)),
         data        => <>);

      Commands_Vector.Append
        (LSP.Structures.Command_Or_CodeAction'
           (Is_Command => False, CodeAction => Code_Action));
   end Append_Code_Action;

   ------------
   -- Create --
   ------------

   overriding
   function Create
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

               else
                  Skip_Value (C);
               end if;
            end;

            Next (C);
         end loop;
         Next (C);
      end return;
   end Create;

   --------------
   -- Refactor --
   --------------

   overriding
   procedure Refactor
     (Self    : Command;
      Handler : not null access LSP.Server_Notification_Receivers.
        Server_Notification_Receiver'Class;
      Client  : not null access LSP.Client_Message_Receivers.
        Client_Message_Receiver'Class;
      Edits   : out LAL_Refactor.Refactoring_Edits)
   is
      use Langkit_Support.Slocs;
      use Libadalang.Analysis;
      use LAL_Refactor;
      use LAL_Refactor.Sort_Dependencies;

      Message_Handler  : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context          : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context).all;
      File             : constant GNATCOLL.VFS.Virtual_File :=
        Message_Handler.To_File (Self.Where.uri);
      Analysis_Unit    : constant Libadalang.Analysis.Analysis_Unit :=
        Context.Get_AU (File);
      Sloc             : constant Source_Location :=
        (Langkit_Support.Slocs.Line_Number
           (Self.Where.a_range.start.line) + 1,
         Langkit_Support.Slocs.Column_Number
           (Self.Where.a_range.start.character) + 1);
      Compilation_Unit : constant Libadalang.Analysis.Compilation_Unit :=
        Analysis_Unit.Root.Lookup (Sloc).P_Enclosing_Compilation_Unit;

      Sorter : constant Dependencies_Sorter :=
        Create_Dependencies_Sorter (Compilation_Unit);
   begin
      Edits := Sorter.Refactor (null);
   end Refactor;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Command'Class;
      Context : LSP.Ada_Contexts.Context;
      Where   : LSP.Structures.Location) is
   begin
      Self.Context := Context.Id;
      Self.Where   := Where;
   end Initialize;

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

      Result.Append (JSON_Stream_Element'(Kind => End_Object));
      Result.Append (JSON_Stream_Element'(Kind => End_Array));

      return Result;
   end Write_Command;

end LSP.Ada_Handlers.Refactor.Sort_Dependencies;
