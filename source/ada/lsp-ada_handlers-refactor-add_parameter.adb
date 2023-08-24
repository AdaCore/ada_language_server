------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021-2023, AdaCore                     --
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

with Libadalang.Analysis; use Libadalang.Analysis;

with LAL_Refactor.Subprogram_Signature;
use LAL_Refactor.Subprogram_Signature;

with VSS.JSON.Streams;
with VSS.Strings.Conversions;

with LSP.Enumerations;
with LSP.Structures.LSPAny_Vectors; use LSP.Structures.LSPAny_Vectors;

package body LSP.Ada_Handlers.Refactor.Add_Parameter is

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self                        : in out Command;
      Context                     : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector             : in out LSP.Structures.
        Command_Or_CodeAction_Vector;
      Where                       : LSP.Structures.Location;
      Requires_Full_Specification : Boolean)
   is
      Code_Action : LSP.Structures.CodeAction;

   begin
      Self.Initialize
        (Context                     => Context.all,
         Where                       => Where,
         Requires_Full_Specification => Requires_Full_Specification);

      Code_Action :=
        (title       => "Add Parameter",
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
              (title      => <>,
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

               if Key = "context_id" then
                  Self.Context_Id := Element (C).String_Value;

               elsif Key = "where" then
                  Self.Where := From_Any (C);

               elsif Key = "newParameter" then
                  Self.New_Parameter := Element (C).String_Value;

               elsif Key = "requiresFullSpecification" then
                  Self.Requires_Full_Specification := From_Any (C);

               else
                  Skip_Value (C);
               end if;
            end;

            Next (C);
         end loop;
      end return;
   end Create;

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
      use Langkit_Support.Slocs;
      use LAL_Refactor;

      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context_Id).all;

      function Analysis_Units return Analysis_Unit_Array is
        (Context.Analysis_Units);
      --  Provides the Context Analysis_Unit_Array to the Mode_Changer

      Adder : constant Parameter_Adder :=
        Create
          (Unit          => Context.Get_AU
             (Context.URI_To_File (Self.Where.uri)),
           Location      =>
             (Langkit_Support.Slocs.Line_Number
                (Self.Where.a_range.start.line) + 1,
              Langkit_Support.Slocs.Column_Number
                (Self.Where.a_range.start.character) + 1),
           New_Parameter =>
             VSS.Strings.Conversions.To_Unbounded_UTF_8_String
               (Self.New_Parameter));
   begin
      Edits := Adder.Refactor (Analysis_Units'Access);
   end Refactor;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                        : in out Command'Class;
      Context                     : LSP.Ada_Contexts.Context;
      Where                       : LSP.Structures.Location;
      Requires_Full_Specification : Boolean) is
   begin
      Self.Context_Id    := Context.Id;
      Self.Where         := Where;
      Self.New_Parameter := VSS.Strings.Empty_Virtual_String;

      Self.Requires_Full_Specification := Requires_Full_Specification;
   end Initialize;

   -------------------
   -- Write_Command --
   -------------------

   function Write_Command
     (Self : Command)
      return LSP.Structures.LSPAny_Vector
   is
      use VSS.JSON.Streams;

      Result : LSP.Structures.LSPAny_Vector;
   begin
      Result.Append (JSON_Stream_Element'(Kind => Start_Object));

      --  "context_id"
      Add_Key ("context_id", Result);
      To_Any (Self.Context_Id, Result);

      --  "where"
      Add_Key ("where", Result);
      To_Any (Self.Where, Result);

      --  "newParameter"
      Add_Key ("newParameter", Result);
      To_Any (Self.New_Parameter, Result);

      --  "requiresFullSpecification"
      Add_Key ("requiresFullSpecification", Result);
      To_Any (Self.Requires_Full_Specification, Result);

      Result.Append (JSON_Stream_Element'(Kind => End_Object));

      return Result;
   end Write_Command;

end LSP.Ada_Handlers.Refactor.Add_Parameter;
