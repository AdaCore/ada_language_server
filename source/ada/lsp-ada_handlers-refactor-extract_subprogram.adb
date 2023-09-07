------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis;   use Libadalang.Analysis;
with Libadalang.Common;     use Libadalang.Common;

with LAL_Refactor; use LAL_Refactor;
with LAL_Refactor.Extract_Subprogram;
use LAL_Refactor.Extract_Subprogram;

with VSS.JSON.Streams;

with LSP.Enumerations;
with LSP.Structures.LSPAny_Vectors; use LSP.Structures.LSPAny_Vectors;

package body LSP.Ada_Handlers.Refactor.Extract_Subprogram is

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self            : in out Command;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Where           : LSP.Structures.Location;
      Subprogram_Kind : Libadalang.Common.Ada_Subp_Kind)
   is
      Code_Action : LSP.Structures.CodeAction;

      Code_Action_Title  : constant String :=
        (if Subprogram_Kind in Ada_Subp_Kind_Procedure_Range then
           "Extract Procedure"
         else
           "Extract Function");

   begin
      Self.Initialize
        (Context         => Context.all,
         Where           => Where,
         Subprogram_Kind => Subprogram_Kind);

      Code_Action :=
        (title       =>
           VSS.Strings.Conversions.To_Virtual_String (Code_Action_Title),
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

               elsif Key = "section_to_extract_sloc" then
                  Self.Section_To_Extract_SLOC := From_Any (C);

               elsif Key = "subprogram_kind" then
                  Self.Subprogram_Kind :=
                    Libadalang.Common.Ada_Subp_Kind'Value
                      (VSS.Strings.Conversions.To_UTF_8_String
                         (Element (C).String_Value));

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
      Client  : not null access LSP.Client_Message_Receivers.
        Client_Message_Receiver'Class;
      Edits   : out LAL_Refactor.Refactoring_Edits)
   is
      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context_Id).all;

      function Analysis_Units return Analysis_Unit_Array is
        (Context.Analysis_Units);
      --  Provides the Context Analysis_Unit_Array to the Mode_Changer

      File            : constant GNATCOLL.VFS.Virtual_File :=
        Message_Handler.To_File (Self.Section_To_Extract_SLOC.uri);

      Unit               : constant Analysis_Unit := Context.Get_AU (File);
      Section_To_Extract : constant Source_Location_Range :=
        (Langkit_Support.Slocs.Line_Number
           (Self.Section_To_Extract_SLOC.a_range.start.line) + 1,
         Langkit_Support.Slocs.Line_Number
           (Self.Section_To_Extract_SLOC.a_range.an_end.line) + 1,
         Column_Number
           (Self.Section_To_Extract_SLOC.a_range.start.character) + 1,
         Column_Number
           (Self.Section_To_Extract_SLOC.a_range.an_end.character) + 1);

      Extractor : constant Subprogram_Extractor :=
        Create_Subprogram_Extractor
          (Unit               => Unit,
           Section_To_Extract => Section_To_Extract,
           Subprogram_Kind    => Self.Subprogram_Kind,
           Subprogram_Name    =>
             Default_Extracted_Subprogram_Name
               (Unit            => Unit,
                Location        =>
                  (Section_To_Extract.Start_Line,
                   Section_To_Extract.Start_Column)));
   begin
      Edits := Extractor.Refactor (Analysis_Units'Access);
   end Refactor;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self             : in out Command'Class;
      Context          : LSP.Ada_Contexts.Context;
      Where            : LSP.Structures.Location;
      Subprogram_Kind  : Libadalang.Common.Ada_Subp_Kind) is
   begin
      Self.Context_Id := Context.Id;
      Self.Section_To_Extract_SLOC := Where;
      Self.Subprogram_Kind := Subprogram_Kind;
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

      --  "context_id"
      Add_Key ("context_id", Result);
      To_Any (Self.Context_Id, Result);

      --  "section_to_extract_sloc"
      Add_Key ("section_to_extract_sloc", Result);
      To_Any (Self.Section_To_Extract_SLOC, Result);

      --  "subprogram_kind"
      Add_Key ("subprogram_kind", Result);
      To_Any
        (VSS.Strings.Conversions.To_Virtual_String
           (Self.Subprogram_Kind'Image),
         Result);

      Result.Append (JSON_Stream_Element'(Kind => End_Object));
      Result.Append (JSON_Stream_Element'(Kind => End_Array));

      return Result;
   end Write_Command;

end LSP.Ada_Handlers.Refactor.Extract_Subprogram;
