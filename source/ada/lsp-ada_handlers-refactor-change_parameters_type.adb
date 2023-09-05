------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2023, AdaCore                      --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Langkit_Support.Slocs;         use Langkit_Support.Slocs;

with Libadalang.Analysis;           use  Libadalang.Analysis;

with LAL_Refactor.Subprogram_Signature.Change_Parameters_Type;

with VSS.JSON.Streams;
with VSS.Strings.Conversions;

with LSP.Enumerations;
with LSP.Structures.LSPAny_Vectors; use LSP.Structures.LSPAny_Vectors;

package body LSP.Ada_Handlers.Refactor.Change_Parameters_Type is

   function To_Virtual_String_Vector
     (Grammar_Rules : Laltools.Common.Grammar_Rule_Vector)
      return VSS.String_Vectors.Virtual_String_Vector;
   --  TODO

   function To_Virtual_String_Vector
     (Grammar_Rules : Laltools.Common.Grammar_Rule_Vector)
      return VSS.String_Vectors.Virtual_String_Vector
   is
      Result : VSS.String_Vectors.Virtual_String_Vector;

   begin
      for Rule of Grammar_Rules loop
         Result.Append
           (VSS.Strings.Conversions.To_Virtual_String (Rule'Image));
      end loop;

      return Result;
   end To_Virtual_String_Vector;

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self            : in out Command;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Where           : LSP.Structures.Location;
      Syntax_Rules    : Laltools.Common.Grammar_Rule_Vector)
   is
      Code_Action : LSP.Structures.CodeAction;

   begin
      Self.Initialize
        (Context             => Context.all,
         Where               => Where,
         Syntax_Rules        => Syntax_Rules,
         New_Parameters_Type => "");

      Code_Action :=
        (title       => VSS.Strings.Conversions.To_Virtual_String
           ("Change Parameter Type"),
         kind        =>
           (Is_Set => True,
            Value  => LSP.Enumerations.RefactorRewrite),
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

               elsif Key = "newParametersType" then
                  Self.New_Parameters_Type := Element (C).String_Value;

               elsif Key = "syntaxRules" then
                  Self.Syntax_Rules := From_Any (C);

               else
                  Skip_Value (C);
               end if;
            end;

            Next (C);
         end loop;
      end return;
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                : in out Command'Class;
      Context             : LSP.Ada_Contexts.Context;
      Where               : LSP.Structures.Location;
      Syntax_Rules        : Laltools.Common.Grammar_Rule_Vector;
      New_Parameters_Type : VSS.Strings.Virtual_String) is
   begin
      Self.Context := Context.Id;
      Self.Where   := Where;

      Self.Syntax_Rules :=
        To_Virtual_String_Vector (Syntax_Rules);
      Self.New_Parameters_Type := New_Parameters_Type;
   end Initialize;

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
      use LAL_Refactor;
      use LAL_Refactor.Subprogram_Signature.Change_Parameters_Type;
      use VSS.Strings.Conversions;

      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context).all;

      Unit : constant Analysis_Unit := Context.LAL_Context.Get_From_File
        (VSS.Strings.Conversions.To_UTF_8_String (Self.Where.uri));

      Parameters_SLOC_Range : constant Source_Location_Range :=
        (Langkit_Support.Slocs.Line_Number
           (Self.Where.a_range.start.line) + 1,
         Langkit_Support.Slocs.Line_Number
           (Self.Where.a_range.an_end.line) + 1,
         Column_Number (Self.Where.a_range.start.character) + 1,
         Column_Number (Self.Where.a_range.an_end.character) + 1);

      function Analysis_Units return Analysis_Unit_Array is
        (Context.Analysis_Units);
      --  Provides the Context Analysis_Unit_Array to the Pull_Upper

      Changer : constant Parameters_Type_Changer :=
        Create_Parameters_Type_Changer
          (Unit                             => Unit,
           Parameters_Source_Location_Range =>
             Parameters_SLOC_Range,
           New_Parameters_Type              =>
             To_Unbounded_UTF_8_String (Self.New_Parameters_Type));

   begin
      Edits := Changer.Refactor (Analysis_Units'Access);
   end Refactor;

   -------------------
   -- Write_Command --
   -------------------

   function Write_Command (Self : Command) return LSP.Structures.LSPAny_Vector
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

      --  "newParametersType"
      Add_Key ("newParametersType", Result);
      To_Any (Self.New_Parameters_Type, Result);

      --  "syntaxRules"
      Add_Key ("syntaxRules", Result);
      To_Any (Self.Syntax_Rules, Result);

      Result.Append (JSON_Stream_Element'(Kind => End_Object));
      Result.Append (JSON_Stream_Element'(Kind => End_Array));

      return Result;
   end Write_Command;

end LSP.Ada_Handlers.Refactor.Change_Parameters_Type;
