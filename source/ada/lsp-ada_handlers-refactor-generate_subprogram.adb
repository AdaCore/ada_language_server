------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2025-2026, AdaCore                  --
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

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;

with LAL_Refactor; use LAL_Refactor;
with LAL_Refactor.Generate_Subprogram;

with LSP.Structures;
with VSS.JSON.Streams;
with VSS.Strings.Conversions;
with LSP.Enumerations;
with LSP.Structures.LSPAny_Vectors; use LSP.Structures.LSPAny_Vectors;

package body LSP.Ada_Handlers.Refactor.Generate_Subprogram is

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self            : in out Command;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Subp_Start      : LSP.Structures.Location;
      Subp_Type       : Ada_Subp_Kind)
   is
      Code_Action : LSP.Structures.CodeAction;

      Code_Action_Title : constant String :=
        (if Subp_Type in Ada_Subp_Kind_Procedure_Range
         then "Generate Procedure Body"
         else "Generate Function Body");

   begin
      Self.Initialize (Context => Context.all, Subp_Start => Subp_Start);

      Code_Action :=
        (title       =>
           VSS.Strings.Conversions.To_Virtual_String (Code_Action_Title),
         kind        =>
           (Is_Set => True, Value => LSP.Enumerations.RefactorRewrite),
         diagnostics => <>,
         edit        => (Is_Set => False),
         isPreferred => (Is_Set => False),
         disabled    => (Is_Set => False),
         command     =>
           (Is_Set => True,
            Value  =>
              (title     => "",
               command   =>
                 VSS.Strings.Conversions.To_Virtual_String
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
     (Any : not null access LSP.Structures.LSPAny_Vector) return Command
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

         while Has_Element (C) and then Element (C).Kind /= End_Object loop
            pragma Assert (Element (C).Kind = Key_Name);
            declare
               Key : constant Virtual_String := Element (C).Key_Name;
            begin
               Next (C);

               if Key = "context_id" then
                  Self.Context_Id := Element (C).String_Value;

               elsif Key = "subp_start" then
                  Self.Subp_Start := From_Any (C);

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

   overriding
   procedure Refactor
     (Self    : Command;
      Handler : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Edits   : out LAL_Refactor.Refactoring_Edits)
   is
      use LAL_Refactor.Generate_Subprogram;

      Message_Handler : LSP.Ada_Handlers.Message_Handler renames
        LSP.Ada_Handlers.Message_Handler (Handler.all);
      Context         : LSP.Ada_Contexts.Context renames
        Message_Handler.Contexts.Get (Self.Context_Id).all;
      File            : constant GNATCOLL.VFS.Virtual_File :=
        Message_Handler.To_File (Self.Subp_Start.uri);
      Dest_Filename   : constant String := File.Display_Full_Name;
      Document        : LSP.Ada_Documents.Document_Access;
      Node            : Ada_Node := No_Ada_Node;
      Target_Subp     : Subp_Decl := No_Subp_Decl;
      --  Get_Subp_Decl will marshall node into a Subp_Decl

      function Analysis_Units return Analysis_Unit_Array
      is (Context.Analysis_Units);
      --  Provide project context for refactor engine
   begin
      Document := Message_Handler.Get_Open_Document (Self.Subp_Start.uri);
      Node := Document.Get_Node_At (Context, Self.Subp_Start.a_range.start);
      Target_Subp := Get_Subp_Decl (Node);
      if Target_Subp.Is_Null then
         Edits.Diagnostics.Append
           (New_Item =>
              Report_Error
                ("The target subprogram could not be resolved precisely.",
                 Node));
      else
         Edits :=
           Create_Subprogram_Generator (Target_Subp).Refactor
             (Analysis_Units'Access);
      end if;
   exception
      when E : others =>
         Message_Handler.Trace_Exception
           (E,
            VSS.Strings.Conversions.To_Virtual_String
              ("Failed to retrieve document or node from LSP range."));
         Edits.Diagnostics.Append
           (New_Item =>
              Report_Error
                (Msg  =>
                   "The target subprogram could not be resolved precisely.",
                 SLOC =>
                   Message_Handler.From_LSP_Range
                     (Context.Get_AU (File), Self.Subp_Start.a_range),
                 File => Dest_Filename));
         Document.Cleanup;
   end Refactor;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : in out Command'Class;
      Context    : LSP.Ada_Contexts.Context;
      Subp_Start : LSP.Structures.Location) is
   begin
      Self.Context_Id := Context.Id;
      Self.Subp_Start := Subp_Start;
   end Initialize;

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

      --  "context_id"
      Add_Key ("context_id", Result);
      To_Any (Self.Context_Id, Result);

      --  "subp_start"
      Add_Key ("subp_start", Result);
      To_Any (Self.Subp_Start, Result);

      Result.Append (JSON_Stream_Element'(Kind => End_Object));
      Result.Append (JSON_Stream_Element'(Kind => End_Array));

      return Result;
   end Write_Command;

end LSP.Ada_Handlers.Refactor.Generate_Subprogram;
