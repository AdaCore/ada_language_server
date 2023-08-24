------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

--  This package provides some utility subprograms.

with GNATCOLL.VFS;

with Libadalang.Common;
with Libadalang.Sources;
with Langkit_Support.Symbols;

with VSS.Strings.Conversions;
with VSS.Strings.Formatters.Integers;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Templates;
with Laltools.Common;

with LSP.Constants;

package body LSP.Utils is

   ------------------
   -- Canonicalize --
   ------------------

   function Canonicalize
     (Text : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is
      use Langkit_Support.Symbols;

      UTF_32 : constant Wide_Wide_String :=
        VSS.Strings.Conversions.To_Wide_Wide_String (Text);
      Result : constant Symbolization_Result :=
        Libadalang.Sources.Canonicalize (UTF_32);

   begin
      if Result.Success then
         return VSS.Strings.To_Virtual_String (Result.Symbol);
      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Canonicalize;

   -------------------
   -- Get_Decl_Kind --
   -------------------

   function Get_Decl_Kind
     (Node         : Libadalang.Analysis.Basic_Decl;
      Ignore_Local : Boolean := False)
        return LSP.Enumerations.SymbolKind
   is
      use Libadalang.Common;
   begin
      case Node.Kind is
         when Ada_Classic_Subp_Decl |
              Ada_Base_Subp_Body |
              Ada_Entry_Body_Range |
              Ada_Entry_Decl_Range |
              Ada_Generic_Subp_Decl_Range |
              Ada_Generic_Subp_Instantiation_Range |
              Ada_Generic_Subp_Renaming_Decl_Range |
              Ada_Subp_Body_Stub_Range  =>
            return LSP.Enumerations.A_Function;

         when Ada_Component_Decl |
              Ada_Discriminant_Spec =>
            return LSP.Enumerations.Field;

         when Ada_Generic_Formal_Obj_Decl |
              Ada_Param_Spec |
              Ada_Exception_Handler |
              Ada_Object_Decl |
              Ada_Extended_Return_Stmt_Object_Decl |
              Ada_Single_Protected_Decl |
              Ada_Single_Task_Decl =>
            return (if Ignore_Local
                    then LSP.Enumerations.A_Null
                    else
                      (if Laltools.Common.Is_Constant (Node)
                       then LSP.Enumerations.A_Constant
                       else LSP.Enumerations.Variable));

         when Ada_Base_Package_Decl |
              Ada_Generic_Formal_Package |
              --  Ignore: Ada_Generic_Package_Decl kind, this node always have
              --  an Ada_Generic_Package_Internal as a child and we will use it
              --  to create the CompletionItem/DocumentSymbol
              Ada_Generic_Package_Instantiation |
              Ada_Generic_Package_Renaming_Decl |
              Ada_Package_Renaming_Decl =>
            return LSP.Enumerations.A_Package;

         when Ada_Package_Body_Stub |
              Ada_Protected_Body_Stub |
              Ada_Task_Body_Stub |
              Ada_Package_Body |
              Ada_Protected_Body |
              Ada_Task_Body =>
            return LSP.Enumerations.Module;

         when Ada_Concrete_Type_Decl |
              Ada_Formal_Type_Decl =>
            return (if Laltools.Common.Is_Structure (Node)
                    then LSP.Enumerations.Struct
                    else LSP.Enumerations.Class);

         when Ada_Generic_Formal_Type_Decl |
              Ada_Classwide_Type_Decl |
              Ada_Incomplete_Type_Decl |
              Ada_Incomplete_Tagged_Type_Decl |
              Ada_Protected_Type_Decl |
              Ada_Task_Type_Decl |
              Ada_Subtype_Decl |
              Ada_Anonymous_Type_Decl |
              Ada_Synth_Anonymous_Type_Decl =>
            return LSP.Enumerations.Class;

         when Ada_Entry_Index_Spec |
              Ada_Number_Decl =>
            return LSP.Enumerations.Number;

         when Ada_Enum_Literal_Decl =>
            return (if Ignore_Local
                    then LSP.Enumerations.A_Null
                    else LSP.Enumerations.Enum);

         when Ada_Exception_Decl =>
            return LSP.Enumerations.String;

         when Ada_For_Loop_Var_Decl |
              Ada_Label_Decl |
              Ada_Named_Stmt_Decl =>
            return (if Ignore_Local
                    then LSP.Enumerations.A_Null
                    else LSP.Enumerations.A_Constant);

         when others =>
            null;
      end case;

      return LSP.Enumerations.A_Null;
   end Get_Decl_Kind;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (Unit : Libadalang.Analysis.Analysis_Unit;
      Span : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Structures.Location is
   begin
      return
        (uri     =>
           (VSS.Strings.Conversions.To_Virtual_String
                (Unit.Get_Filename) with null record),
         a_range => To_Range (Span));
   end Get_Location;

   -----------------------
   -- Get_Node_Location --
   -----------------------

   function Get_Node_Location
     (Node : Libadalang.Analysis.Ada_Node'Class)
      return LSP.Structures.Location is
   begin
      return Get_Location (Node.Unit, Node.Sloc_Range);
   end Get_Node_Location;

   -------------------------
   -- Node_Location_Image --
   -------------------------

   function Node_Location_Image
     (Node : Libadalang.Analysis.Ada_Node'Class)
      return VSS.Strings.Virtual_String
   is
      File     : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create_From_UTF8 (Node.Unit.Get_Filename);
      Template : constant VSS.Strings.Templates.Virtual_String_Template :=
        "at {} ({}:{})";

   begin
      return
        Template.Format
          (VSS.Strings.Formatters.Strings.Image
             (VSS.Strings.Conversions.To_Virtual_String
                (File.Display_Base_Name)),
           VSS.Strings.Formatters.Integers.Image
             (Integer (Node.Sloc_Range.Start_Line)),
           VSS.Strings.Formatters.Integers.Image
             (Integer (Node.Sloc_Range.Start_Column)));
   end Node_Location_Image;

   --------------
   -- To_Range --
   --------------

   function To_Range
     (Value : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Structures.A_Range
   is
      use type Langkit_Support.Slocs.Source_Location_Range;

      Result : constant LSP.Structures.A_Range :=
        (if Value = Langkit_Support.Slocs.No_Source_Location_Range then
           LSP.Constants.Empty
         else
           (start =>
                (line      => Natural (Value.Start_Line) - 1,
                 character => Natural --  FIXME (UTF16 index)!
                   (Value.Start_Column) - 1),
            an_end =>
              (line      => Natural (Value.End_Line) - 1,
               character => Natural --  FIXME (UTF16 index)!
                 (Value.End_Column) - 1)));
         --  XXX Code unit offset computation is incorrect here

   begin
      return Result;
   end To_Range;

end LSP.Utils;
