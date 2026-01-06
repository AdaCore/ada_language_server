------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2025, AdaCore                  --
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

with VSS.JSON.Streams;

with Libadalang.Analysis;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with LAL_Refactor; use LAL_Refactor;
with LAL_Refactor.Generate_Package;

with LSP.Structures;
with LSP.Structures.LSPAny_Vectors; use LSP.Structures.LSPAny_Vectors;

package body LSP.Ada_Handlers.Refactor.Generate_Package is

   ------------------------
   -- Append_Code_Action --
   ------------------------

   procedure Append_Code_Action
     (Self            : in out Command;
      Context         : LSP.Ada_Context_Sets.Context_Access;
      Commands_Vector : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Spec_Loc        : LSP.Structures.Location;
      Body_Path       : VSS.Strings.Virtual_String;
      New_File        : Boolean)
   is
      Code_Action : LSP.Structures.CodeAction;
      Command_Title : constant String :=
        (if New_File then Self.Name else "Update Package Body");
   begin
      Self.Initialize (Context.all, Spec_Loc, Body_Path);

      Code_Action :=
        (title       =>
           VSS.Strings.Conversions.To_Virtual_String (Command_Title),
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
               elsif Key = "spec_loc" then
                  Self.Spec_Loc := From_Any (C);
               elsif Key = "body_path" then
                  Self.Body_Path := Element (C).String_Value;
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
      Edits   : out Refactoring_Edits)
   is
      use Libadalang.Analysis;
      use LAL_Refactor.Generate_Package;

      Context   : LSP.Ada_Contexts.Context renames
        Handler.Contexts.Get (Self.Context_Id).all;
      Document  : constant LSP.Ada_Documents.Document_Access :=
        Handler.Get_Open_Document (Self.Spec_Loc.uri);
      Node      : constant Ada_Node :=
        Document.Get_Node_At (Context, Self.Spec_Loc.a_range.start);
      Spec      : constant Base_Package_Decl := To_Package_Decl (Node);

      function Analysis_Units return Analysis_Unit_Array
      is (Context.Analysis_Units);
   begin
      if Spec.Is_Null then
         Edits.Diagnostics.Append
           (Problem
              ("Target package specification could not be precisely located.",
               Node.Unit.Get_Filename,
               Handler.From_LSP_Range (Node.Unit, Self.Spec_Loc.a_range)));
      else
         Edits :=
           Build_Package_Generator (Spec).Refactor
             (Analysis_Units'Access);
      end if;
   end Refactor;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : in out Command'Class;
      Context   : LSP.Ada_Contexts.Context;
      Spec_Loc  : LSP.Structures.Location;
      Body_Path : VSS.Strings.Virtual_String) is
   begin
      Self.Context_Id := Context.Id;
      Self.Spec_Loc := Spec_Loc;
      Self.Body_Path := Body_Path;
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

      --  "spec_loc"
      Add_Key ("spec_loc", Result);
      To_Any (Self.Spec_Loc, Result);

      --  "body_path"
      Add_Key ("body_path", Result);
      To_Any (Self.Body_Path, Result);

      Result.Append (JSON_Stream_Element'(Kind => End_Object));
      Result.Append (JSON_Stream_Element'(Kind => End_Array));

      return Result;
   end Write_Command;

end LSP.Ada_Handlers.Refactor.Generate_Package;
