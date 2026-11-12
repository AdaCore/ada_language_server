------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

with GPR2.Build.Compilation_Unit;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.View_Ids;

with Laltools.Common;
with Libadalang.Analysis;
with Libadalang.Common;

with VSS.JSON.Streams;
with VSS.Strings.Conversions;

with LSP.Ada_Context_Sets;
with LSP.Enumerations;
with LSP.Structures.LSPAny_Vectors;
with LSP.Utils;

package body LSP.Ada_Handlers.Unit_Hierarchy_Commands is

   ------------
   -- Create --
   ------------

   overriding
   function Create
     (Any : not null access LSP.Structures.LSPAny_Vector) return Command
   is
      pragma Unreferenced (Any);
   begin
      return (LSP.Ada_Commands.Command with null record);
   end Create;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute
     (Self     : Command;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Response : in out LSP.Structures.LSPAny_Or_Null;
      Error    : in out LSP.Errors.ResponseError_Optional)
   is
      pragma Unreferenced (Self, Error);

      use GPR2;
      use Libadalang.Analysis;
      use Libadalang.Common;
      use VSS.JSON.Streams;
      use LSP.Structures.LSPAny_Vectors;
      use type LSP.Enumerations.SymbolKind;

      Result : LSP.Structures.LSPAny_Vector;

      procedure Append (Item : JSON_Stream_Element);
      --  Append a JSON stream element to the result

      procedure String_Field (Key, Value : String);
      --  Append a string field

      procedure Boolean_Field (Key : String; Value : Boolean);
      --  Append a boolean field

      procedure Write_Project (View : GPR2.Project.View.Object);
      --  Append project metadata, including runtime and external build flags

      function Declaration
        (Path : GPR2.Path_Name.Object; Index : GPR2.Unit_Index)
         return Basic_Decl;
      --  Find the declaration for a source part, including multi-unit files

      function Declaration
        (Part : GPR2.Build.Compilation_Unit.Unit_Location) return Basic_Decl
      is (Declaration (Part.Source, Part.Index));
      --  Find the declaration for a GPR2 unit location

      procedure Write_Part
        (Kind     : GPR2.Unit_Kind;
         View     : GPR2.Project.View.Object;
         Path     : GPR2.Path_Name.Object;
         Index    : GPR2.Unit_Index;
         Sep_Name : GPR2.Optional_Name_Type);
      --  Append a source part and its navigation ranges when available

      procedure Write_Unit (Unit : GPR2.Build.Compilation_Unit.Object);
      --  Append one unit in its namespace

      ------------
      -- Append --
      ------------

      procedure Append (Item : JSON_Stream_Element) is
      begin
         Result.Append (Item);
      end Append;

      ------------------
      -- String_Field --
      ------------------

      procedure String_Field (Key, Value : String) is
      begin
         Add_Key (VSS.Strings.Conversions.To_Virtual_String (Key), Result);
         To_Any (VSS.Strings.Conversions.To_Virtual_String (Value), Result);
      end String_Field;

      -------------------
      -- Boolean_Field --
      -------------------

      procedure Boolean_Field (Key : String; Value : Boolean) is
      begin
         Add_Key (VSS.Strings.Conversions.To_Virtual_String (Key), Result);
         To_Any (Value, Result);
      end Boolean_Field;

      -------------------
      -- Write_Project --
      -------------------

      procedure Write_Project (View : GPR2.Project.View.Object) is
      begin
         Append ((Kind => Start_Object));
         String_Field ("id", String (GPR2.View_Ids.Image (View.Id)));
         String_Field ("name", String (View.Name));
         if View.Path_Name.Is_Defined then
            Add_Key ("uri", Result);
            To_Any (LSP.Utils.To_URI (View.Path_Name), Result);
         end if;
         Boolean_Field ("is-runtime", View.Is_Runtime);
         Boolean_Field ("is-externally-built", View.Is_Externally_Built);
         Append ((Kind => End_Object));
      end Write_Project;

      -----------------
      -- Declaration --
      -----------------

      function Declaration
        (Path : GPR2.Path_Name.Object; Index : GPR2.Unit_Index)
         return Basic_Decl
      is
         Context : constant LSP.Ada_Context_Sets.Context_Access :=
           Handler.Get_Best_Context (LSP.Utils.To_URI (Path));
         Units   : constant Laltools.Common.Compilation_Unit_Vector :=
           Laltools.Common.Get_Compilation_Units
             (Context.Get_AU (Path.Virtual_File));
         Offset  : constant Natural :=
           (if Index = No_Index then 0 else Natural (Index) - 1);
      begin
         --  GPR2 uses No_Index for a single-unit source and one-based indices
         --  for multi-unit sources. Select the first compilation unit for
         --  No_Index, using the vector's own lower bound in both cases.
         if Offset < Natural (Units.Length) then
            return Units.Element (Units.First_Index + Offset).P_Decl;
         else
            return No_Basic_Decl;
         end if;
      exception
         when E : Property_Error =>
            Handler.Trace_Exception (E);
            return No_Basic_Decl;
      end Declaration;

      ----------------
      -- Write_Part --
      ----------------

      procedure Write_Part
        (Kind     : GPR2.Unit_Kind;
         View     : GPR2.Project.View.Object;
         Path     : GPR2.Path_Name.Object;
         Index    : GPR2.Unit_Index;
         Sep_Name : GPR2.Optional_Name_Type)
      is
         Decl : constant Basic_Decl := Declaration (Path, Index);
      begin
         Append ((Kind => Start_Object));
         String_Field
           ("kind",
            (case Kind is
               when S_Spec     => "specification",
               when S_Body     => "body",
               when S_Separate => "separate",
               when others     => "unknown"));
         Add_Key ("uri", Result);
         To_Any (LSP.Utils.To_URI (Path), Result);
         Add_Key ("unit-index", Result);
         To_Any (Integer (Index), Result);
         Add_Key ("project", Result);
         Write_Project (View);

         if Kind = S_Separate then
            String_Field ("separate-name", String (Sep_Name));
         end if;

         if not Decl.Is_Null then
            declare
               Span      : constant LSP.Structures.A_Range :=
                 Handler.To_LSP_Range (Decl);
               Name      : constant Defining_Name := Decl.P_Defining_Name;
               Selection : constant LSP.Structures.A_Range :=
                 (if Name.Is_Null then Span else Handler.To_LSP_Range (Name));
            begin
               Add_Key ("range", Result);
               To_Any (Span, Result);
               Add_Key ("selection-range", Result);
               To_Any (Selection, Result);
            exception
               when E : Property_Error =>
                  Handler.Trace_Exception (E);
            end;
         end if;
         Append ((Kind => End_Object));
      end Write_Part;

      ----------------
      -- Write_Unit --
      ----------------

      procedure Write_Unit (Unit : GPR2.Build.Compilation_Unit.Object) is
         Name   : constant String := String (Unit.Name);
         Parent : constant String := String (GPR2.Parent_Name (Unit.Name));
         Decl   : Basic_Decl := No_Basic_Decl;
      begin
         --  Prefer the specification to preserve generic declaration flags.
         --  GPR2's Main_Part prefers the body, so select the part explicitly.
         --  All parts are listed below, regardless of this choice.
         if Unit.Has_Part (S_Spec) then
            Decl := Declaration (Unit.Spec);
         elsif Unit.Has_Part (S_Body) then
            Decl := Declaration (Unit.Main_Body);
         end if;

         Append ((Kind => Start_Object));
         String_Field
           ("namespace-id", String (GPR2.View_Ids.Image (Unit.Root_View.Id)));
         String_Field ("name", Name);
         String_Field
           ("simple-name",
            (if Parent = "" then Name
             else Name (Name'First + Parent'Length + 1 .. Name'Last)));
         String_Field ("parent-name", Parent);

         if Decl.Is_Null then
            String_Field ("kind", "unknown");
         else
            String_Field
              ("kind",
               (if Decl.Kind
                   in Ada_Package_Decl
                    | Ada_Package_Body
                    | Ada_Generic_Package_Decl
                    | Ada_Generic_Package_Instantiation
                    | Ada_Package_Renaming_Decl
                    | Ada_Generic_Package_Renaming_Decl
                then "package"
                elsif LSP.Utils.Get_Decl_Kind (Decl)
                  = LSP.Enumerations.A_Function
                then "subprogram"
                else "unknown"));
            Boolean_Field
              ("is-generic",
               Decl.Kind
               in Libadalang.Common.Ada_Generic_Decl
                | Ada_Generic_Renaming_Decl);
            Boolean_Field
              ("is-instantiation", Decl.Kind in Ada_Generic_Instantiation);
            Boolean_Field
              ("is-renaming",
               Decl.Kind
               in Ada_Package_Renaming_Decl
                | Ada_Subp_Renaming_Decl
                | Ada_Generic_Renaming_Decl);
         end if;

         Add_Key ("parts", Result);
         Append ((Kind => Start_Array));
         Unit.For_All_Part (Write_Part'Access);
         Append ((Kind => End_Array));
         Append ((Kind => End_Object));
      end Write_Unit;

   begin
      if not Handler.Project_Tree_Is_Defined then
         Response := (Is_Null => True);
         return;
      end if;

      Append ((Kind => Start_Object));
      Add_Key ("root-project", Result);
      Write_Project (Handler.Project_Tree.Root_Project);
      Add_Key ("namespaces", Result);
      Append ((Kind => Start_Array));
      for View of Handler.Project_Tree.Namespace_Root_Projects loop
         Write_Project (View);
      end loop;
      Append ((Kind => End_Array));
      Add_Key ("units", Result);
      Append ((Kind => Start_Array));
      for View of Handler.Project_Tree.Namespace_Root_Projects loop
         for Unit of View.Units (With_Externally_Built => True) loop
            if not Unit.Is_Empty then
               Write_Unit (Unit);
            end if;
         end loop;
      end loop;
      Append ((Kind => End_Array));
      Append ((Kind => End_Object));
      Response := (Is_Null => False, Value => Result);
   end Execute;

end LSP.Ada_Handlers.Unit_Hierarchy_Commands;
