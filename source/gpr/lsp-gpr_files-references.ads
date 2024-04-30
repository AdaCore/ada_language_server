------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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
--
--  This package provides GPR references API.

with Gpr_Parser.Common;

with GPR2.Project.Attribute_Index;

package LSP.GPR_Files.References is

   type Reference is private;

   type Ref_Kind is
     (No_Ref, Project_Ref, Type_Ref, Variable_Ref, Attribute_Ref, Package_Ref);

   function Kind (Ref : Reference) return Ref_Kind;

   function Token_Reference
     (File     : LSP.GPR_Files.File_Access;
      Position : LSP.Structures.Position)
      return Gpr_Parser.Common.Token_Reference;

   function Identifier_Reference
     (File            : LSP.GPR_Files.File_Access;
      Current_Package : GPR2.Package_Id;
      Token           : Gpr_Parser.Common.Token_Reference)
      return Reference;
   --  return the Project, Type, Variable, Attribute, Package reference
   --  found at 'Location'. 'Current_Package' useful for package's variable.

   function Referenced_File
     (File      : LSP.GPR_Files.File_Access;
      Reference : LSP.GPR_Files.References.Reference)
      return LSP.GPR_Files.File_Access;

   function Referenced_Package
     (Reference : LSP.GPR_Files.References.Reference)
      return GPR2.Package_Id;

   function Referenced_Variable
     (Reference : LSP.GPR_Files.References.Reference)
      return LSP.GPR_Files.Variable_Id;

   function Has_Project
     (Reference : LSP.GPR_Files.References.Reference) return Boolean;

   function Is_Project_Reference
     (Reference : LSP.GPR_Files.References.Reference) return Boolean;

   function Has_Package
     (Reference : LSP.GPR_Files.References.Reference) return Boolean;

   function Is_Package_Reference
     (Reference : LSP.GPR_Files.References.Reference) return Boolean;

   function Is_Type_Reference
     (Reference : LSP.GPR_Files.References.Reference) return Boolean;

   function Is_Variable_Reference
     (Reference : LSP.GPR_Files.References.Reference) return Boolean;

   function Is_Attribute_Reference
     (Reference : LSP.GPR_Files.References.Reference) return Boolean;

   function In_Type_Reference
     (Reference : LSP.GPR_Files.References.Reference) return Boolean;

   No_Reference : constant Reference;
   --  No reference found

   function Get_Referenced_File
     (File : LSP.GPR_Files.File_Access;
      Reference : LSP.GPR_Files.References.Reference)
      return LSP.GPR_Files.File_Access;

   type Attribute_Definition is record
      Name   : GPR2.Q_Attribute_Id;
      Index  : GPR2.Project.Attribute_Index.Object;
      At_Pos : GPR2.Unit_Index;
   end record;
   --  contains all information required to access a project attribute.

   No_Attribute_Definition : constant Attribute_Definition :=
                               ((GPR2.Project_Level_Scope,
                                +"No_Attribute_Definition"),
                                GPR2.Project.Attribute_Index.Undefined,
                                GPR2.No_Index);

   function Referenced_Attribute
     (Reference : LSP.GPR_Files.References.Reference)
      return Attribute_Definition;
   --  if Reference is a valid attribute reference returns all GPR2
   --  informations needed to access attribute value.

private

   package GPC renames Gpr_Parser.Common;

   type Reference (Kind : Ref_Kind := No_Ref) is record
      Token : Gpr_Parser.Common.Token_Reference := GPC.No_Token;
      case Kind is
         when No_Ref =>
            null;
         when others =>
            Project           : Project_Id := No_Project;
            In_Type_Reference : Boolean := False;
            case Kind is
               when Project_Ref =>
                  null;
               when Type_Ref =>
                  Typ : Type_Id := No_Type;
               when others =>
                  Pack : GPR2.Package_Id := GPR2.Project_Level_Scope;
                  case Kind is
                     when Variable_Ref =>
                        Variable  : Variable_Id := No_Variable;
                     when Attribute_Ref =>
                        Attribute : GPR2.Optional_Attribute_Id :=
                                      GPR2.No_Attribute;
                        Index     : Index_Type := No_Index;
                     when others =>
                        null;
                  end case;
            end case;
      end case;
   end record;

   No_Reference : constant Reference := (Kind => No_Ref, Token => GPC.No_Token);
   --  No reference found

   function Has_Project
     (Reference : LSP.GPR_Files.References.Reference) return Boolean is
      (Reference.Kind /= No_Ref);

   function Is_Project_Reference
     (Reference : LSP.GPR_Files.References.Reference) return Boolean is
      (Reference.Kind = Project_Ref);

   function Has_Package
     (Reference : LSP.GPR_Files.References.Reference) return Boolean is
      (Reference.Kind not in No_Ref | Project_Ref | Type_Ref);

   function Is_Package_Reference
     (Reference : LSP.GPR_Files.References.Reference) return Boolean is
      (Reference.Kind = Package_Ref);

   function Is_Type_Reference
     (Reference : LSP.GPR_Files.References.Reference) return Boolean is
     (Reference.Kind = Type_Ref);

   function Is_Variable_Reference
     (Reference : LSP.GPR_Files.References.Reference) return Boolean is
     (Reference.Kind = Variable_Ref);

   function Is_Attribute_Reference
     (Reference : LSP.GPR_Files.References.Reference) return Boolean is
     (Reference.Kind = Attribute_Ref);

   function In_Type_Reference
     (Reference : LSP.GPR_Files.References.Reference) return Boolean is
     (if Reference.Kind in Project_Ref | Type_Ref | Package_Ref
      then Reference.In_Type_Reference
      else False);

   function Referenced_Variable
     (Reference : LSP.GPR_Files.References.Reference)
      return LSP.GPR_Files.Variable_Id is
     (if Reference.Kind = Variable_Ref
      then Reference.Variable
      else No_Variable);

end LSP.GPR_Files.References;
