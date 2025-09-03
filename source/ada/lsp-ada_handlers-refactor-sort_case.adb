------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2025, AdaCore                       --
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

with LAL_Refactor;           use LAL_Refactor;
with LAL_Refactor.Sort_Case; use LAL_Refactor.Sort_Case;

with VSS.JSON.Streams;

with LSP.Enumerations;
with LSP.Structures.LSPAny_Vectors; use LSP.Structures.LSPAny_Vectors;

package body LSP.Ada_Handlers.Refactor.Sort_Case is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Base_Command'Class;
      Context : LSP.Ada_Contexts.Context;
      Where   : LSP.Structures.Location) is
   begin
      Self.Context_Id := Context.Id;
      Self.Location   := Where;
   end Initialize;

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self            : in out Base_Command'Class;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Where           : LSP.Structures.Location;
      Tag             : String)
   is
      Code_Action : LSP.Structures.CodeAction;

   begin
      Self.Initialize
        (Context => Context.all,
         Where   => Where);

      Code_Action :=
        (title       =>
           VSS.Strings.Conversions.To_Virtual_String (Self.Name),
         kind        =>
           (Is_Set => True,
            Value  => LSP.Enumerations.RefactorExtract),
         diagnostics => <>,
         edit        => (Is_Set => False),
         isPreferred => (Is_Set => False),
         disabled    => (Is_Set => False),
         command     =>
           (Is_Set => True,
            Value  =>
              (title     => "",
               command   => VSS.Strings.Conversions.To_Virtual_String (Tag),
               arguments => Self.Write_Command)),
         data        => <>);

      Commands_Vector.Append
        (LSP.Structures.Command_Or_CodeAction'
           (Is_Command => False, CodeAction => Code_Action));
   end Append_Code_Action;

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self            : in out Alphabetical_Command'Class;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Where           : LSP.Structures.Location) is
   begin
      Append_Code_Action
        (Self, Context, Commands_Vector, Where,
         Alphabetical_Command'External_Tag);
   end Append_Code_Action;

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self            : in out Declaration_Command'Class;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Where           : LSP.Structures.Location) is
   begin
      Append_Code_Action
        (Self, Context, Commands_Vector, Where,
         Declaration_Command'External_Tag);
   end Append_Code_Action;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Alphabetical_Command is
   begin
      return Self : Alphabetical_Command do
         Load (Self, Any);
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Declaration_Command is
   begin
      return Self : Declaration_Command do
         Load (Self, Any);
      end return;
   end Create;

   ----------
   -- Load --
   ----------

   procedure Load
     (Self : in out Base_Command'Class;
      Any  : not null access LSP.Structures.LSPAny_Vector)
   is
      use VSS.JSON.Streams;
      use VSS.Strings;
      use LSP.Structures.JSON_Event_Vectors;

      C : Cursor := Any.First;
   begin
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

            if Key = "context_id" then
               Self.Context_Id := Element (C).String_Value;

            elsif Key = "location" then
               Self.Location := From_Any (C);

            else
               Skip_Value (C);
            end if;
         end;

         Next (C);
      end loop;
   end Load;

   ---------------------------
   -- Prepare_Refactor_Data --
   ---------------------------

   procedure Prepare_Refactor_Data
     (Self     : Base_Command'Class;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Unit     : out Analysis_Unit;
      Location : out Source_Location)
   is
      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context_Id).all;
      File            : constant GNATCOLL.VFS.Virtual_File :=
        Message_Handler.To_File (Self.Location.uri);
   begin
      Unit      := Context.Get_AU (File);
      Location  := Start_Sloc
        (From_LSP_Range (Message_Handler, Unit, Self.Location.a_range));
   end Prepare_Refactor_Data;

   --------------
   -- Refactor --
   --------------

   overriding procedure Refactor
     (Self    : Alphabetical_Command;
      Handler : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Edits   : out LAL_Refactor.Refactoring_Edits)
   is
      Unit     : Analysis_Unit;
      Location : Source_Location;
   begin
      Self.Prepare_Refactor_Data (Handler, Unit, Location);
      declare
         Extractor : constant Alphabetical_Case_Sorter :=
           Create_Alphabetical_Case_Sorter
             (Unit     => Unit,
              Location => Location);
      begin
         Edits := Extractor.Refactor (null);
      end;
   end Refactor;

   --------------
   -- Refactor --
   --------------

   overriding procedure Refactor
     (Self    : Declaration_Command;
      Handler : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Edits   : out LAL_Refactor.Refactoring_Edits)
   is
      Unit     : Analysis_Unit;
      Location : Source_Location;
   begin
      Self.Prepare_Refactor_Data (Handler, Unit, Location);
      declare
         Extractor : constant Declaration_Case_Sorter :=
           Create_Declaration_Case_Sorter
             (Unit     => Unit,
              Location => Location);
      begin
         Edits := Extractor.Refactor (null);
      end;
   end Refactor;

   -------------------
   -- Write_Command --
   -------------------

   function Write_Command
     (Self : Base_Command'Class) return LSP.Structures.LSPAny_Vector
   is
      use VSS.JSON.Streams;

      Result : LSP.Structures.LSPAny_Vector;
   begin
      Result.Append (JSON_Stream_Element'(Kind => Start_Array));
      Result.Append (JSON_Stream_Element'(Kind => Start_Object));

      --  "context_id"
      Add_Key ("context_id", Result);
      To_Any (Self.Context_Id, Result);

      --  "location"
      Add_Key ("location", Result);
      To_Any (Self.Location, Result);

      Result.Append (JSON_Stream_Element'(Kind => End_Object));
      Result.Append (JSON_Stream_Element'(Kind => End_Array));

      return Result;
   end Write_Command;

end LSP.Ada_Handlers.Refactor.Sort_Case;
