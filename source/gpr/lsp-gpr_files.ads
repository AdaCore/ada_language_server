------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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
--  This package provides a Gpr file abstraction.

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with Ada.Strings.Equal_Case_Insensitive;

with GNATCOLL.VFS;

with GPR2.Environment;
with GPR2.File_Readers;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Registry.Attribute;

with Gpr_Parser.Analysis;
with Gpr_Parser.Common;
with Gpr_Parser_Support.Slocs;
with Gpr_Parser_Support.Text;
with Gpr_Parser_Support.Token_Data_Handlers;

with Langkit_Support.Slocs;

with LSP.Text_Documents.Langkit_Documents;

with VSS.Strings;
with VSS.Strings.Hash;
with VSS.String_Vectors;

with LSP.Structures;
with LSP.Tracers;

package LSP.GPR_Files is

   use GPR2; --  temporary workaround of a coverage instrumentation problem

   type File (Tracer : not null LSP.Tracers.Tracer_Access) is
     tagged limited private;
   --  A parsed GPR file.

   type File_Access is access all LSP.GPR_Files.File
     with Storage_Size => 0;

   -------------------
   -- File_Provider --
   -------------------

   type File_Provider is limited interface;

   type File_Provider_Access is access all File_Provider'Class;
   --  A File_Provider is an object that contains files and
   --  is able to retrieve a document from its given URI.

   function Is_Openened_Document
     (Self : access File_Provider;
      File : GNATCOLL.VFS.Virtual_File) return Boolean is abstract;
   --  Return True if file currently opened in client

   function Follow_Symlinks
     (Self : access File_Provider) return Boolean is abstract;

   function Get_Parsed_File
     (Self  : access File_Provider;
      Path  : GPR2.Path_Name.Object)
      return File_Access is abstract;
   --  Return the file structure for the given URI.
   --  Caller should then parse it if needed

   function Get_File_Reader
     (Self : access File_Provider)
      return GPR2.File_Readers.File_Reader_Reference is abstract;
   --  Reader used to access opened document buffer or disk.

   function To_File
     (Self : access File_Provider;
      Item : LSP.Structures.DocumentUri)
      return GNATCOLL.VFS.Virtual_File is abstract;
   --  Turn URI into Virtual_File

   function To_File
     (Self : access File_Provider;
      Item : LSP.Structures.DocumentUri)
      return GPR2.Path_Name.Object is abstract;
   --  Turn URI into GPR2 path object.

   function To_URI
     (Self : access File_Provider;
      Item : GPR2.Path_Name.Object)
      return LSP.Structures.DocumentUri is abstract;
   --  Turn GPR2 path object into URI.

   procedure Initialize
     (Self          : in out File;
      Path          : GPR2.Path_Name.Object;
      Tab_Stop      : Positive := Gpr_Parser_Support.Slocs.Default_Tab_Stop;
      File_Provider : File_Provider_Access);
   --  Create a new file.

   procedure Parse_Modified_Document
     (File_Provider : File_Provider_Access;
      Path          : GPR2.Path_Name.Object);
   --  parse file in any case

   procedure Cleanup (Self : in out File);
   --  Free all the data associated to this document.

   function Parse
     (File_Provider : File_Provider_Access;
      Path : GPR2.Path_Name.Object) return File_Access;
   --  parse file if not yet parsed

   function Get_Package
     (Self : File; Position : LSP.Structures.Position) return GPR2.Package_Id;
   --  return the Position's Package_Id. Returns Project_Level_Scope if
   --  Position not inside a package.

   function Kind (Self : File) return GPR2.Project_Kind;
   --  return project kind as defined in project qualifier

   function Token
     (Self     : File;
      Location : Gpr_Parser.Slocs.Source_Location)
         return Gpr_Parser.Common.Token_Reference;
   --  Return File's Token at Position.

   function In_Packages
     (Self  : File;
      Name : GPR2.Package_Id) return Boolean;
   --  Return True is Name is part of File's packages.

   function Position_Is_In_Comment
     (Token    : Gpr_Parser.Common.Token_Reference;
      Location : Gpr_Parser.Slocs.Source_Location) return Boolean;
   --  Return True if Token's Position is inside a comment

   function Token_In_Import_Partition
     (Self  : File;
      Token : Gpr_Parser.Common.Token_Reference) return Boolean;

   function Token_At_Import_Partition_End
     (Self  : File;
      Token : Gpr_Parser.Common.Token_Reference) return Boolean;

   function Get_Line (Self : File; Line_Number : Integer)
                      return VSS.Strings.Virtual_String;
   --  Get the 'Self' file line at 'Line_Number'

   function Get_Line
     (Self : File; Line_Number : Gpr_Parser.Slocs.Line_Number)
      return VSS.Strings.Virtual_String is
     (Get_Line (Self, Integer (Line_Number)));

   function Get_Line
     (Self : File; Line_Number : Langkit_Support.Slocs.Line_Number)
      return VSS.Strings.Virtual_String is
     (Get_Line (Self, Integer (Line_Number)));
   --  Get the 'Self' file line at 'Line_Number'

   -------------------------------------------------
   -- GPR Parser / LSP Slocs-Position conversions --
   -------------------------------------------------

   use Gpr_Parser_Support.Slocs;

   function To_Langkit_Location
     (Source_Location : Gpr_Parser.Slocs.Source_Location)
      return Langkit_Support.Slocs.Source_Location is
     (Line => Langkit_Support.Slocs.Line_Number (Source_Location.Line),
      Column => Langkit_Support.Slocs.Column_Number (Source_Location.Column));

   function To_Langkit_Location
     (Source_Location : Langkit_Support.Slocs.Source_Location)
      return Gpr_Parser.Slocs.Source_Location is
     (Line => Gpr_Parser.Slocs.Line_Number (Source_Location.Line),
      Column => Gpr_Parser.Slocs.Column_Number (Source_Location.Column));

   function To_Line_Number
     (Line : Natural) return Gpr_Parser_Support.Slocs.Line_Number is
     (Gpr_Parser_Support.Slocs.Line_Number
        (LSP.Text_Documents.Langkit_Documents.To_Source_Line (Line)));

   function At_Start
     (Sloc_Range : Gpr_Parser_Support.Slocs.Source_Location_Range;
      Location : Gpr_Parser_Support.Slocs.Source_Location) return Boolean is
      (Sloc_Range.Start_Line = Location.Line and then
        Sloc_Range.Start_Column = Location.Column);
   --  Return True if Position at Sloc_Range's beginning

   function At_End
     (Sloc_Range : Gpr_Parser_Support.Slocs.Source_Location_Range;
      Location   : Gpr_Parser_Support.Slocs.Source_Location) return Boolean is
     (Sloc_Range.End_Line = Location.Line and then
      Sloc_Range.End_Column = Location.Column);
   --  Return True if Position at Sloc_Range's end

   function Get_Referenced_GPR
     (File  : LSP.GPR_Files.File;
      Token : Gpr_Parser.Common.Token_Reference) return Path_Name.Object;
   --  find file pointed by gpr token useful for imported & extended files

private

   type Source_Position is record
      Line   : Integer;
      Column : Integer;
   end record;
   --  a line/column position in a gpr file

   type Symbol_Kind is
     (K_Imported,
      K_Project,
      K_Package,
      K_Type,
      K_Variable,
      K_Attribute,
      K_Case,
      K_When);

   --  symbol's type in a gpr document.

   subtype Symbol_Id is Integer;
   Invalid_Symbol_Id : constant Symbol_Id := 0;
   With_Clauses_Symbol_Id : constant Symbol_Id := 1;
   Project_Symbol_Id : constant Symbol_Id := With_Clauses_Symbol_Id + 1;
   Next_Symbol_Id : Symbol_Id := Project_Symbol_Id + 1;

   type Symbol is record
      Id        : Symbol_Id := Invalid_Symbol_Id;
      Parent_Id : Symbol_Id := Invalid_Symbol_Id;
      Ref       : Gpr_Parser.Common.Token_Reference :=
                    Gpr_Parser.Common.No_Token;
      Kind      : Symbol_Kind := K_Project;
      Name      : VSS.Strings.Virtual_String :=
                    VSS.Strings.Empty_Virtual_String;
      A_Range   : LSP.Structures.A_Range;
   end record;

   No_Symbol : constant Symbol :=
                 (Id        => Invalid_Symbol_Id,
                  Parent_Id => Invalid_Symbol_Id,
                  Ref       => Gpr_Parser.Common.No_Token,
                  Kind      => K_Project,
                  Name      => VSS.Strings.Empty_Virtual_String,
                  A_Range   => ((0, 0), (0, 0)));

   use type Gpr_Parser.Common.Token_Reference;

   package Symbol_Lists is
     new Ada.Containers.Vectors (Positive, Symbol);
   subtype Symbol_List is Symbol_Lists.Vector;
   --  with clause's, project's, package's, case & when symbols

   use type Symbol_List;

   package Symbol_List_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Symbol_Id,
      Element_Type => Symbol_List);
   subtype Symbol_List_Map is Symbol_List_Maps.Map;

   package Symbol_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Symbol_Id,
      Element_Type => Symbol);
   subtype Symbol_Map is Symbol_Maps.Map;

   type GPR_Symbols is record
      Imported_Symbols : Symbol_List;
      --  with clauses symbols

      Project          : Symbol;
      --  root projet symbol

      Symbols          : Symbol_Map;
      --  GPR file symbols

      Children_Map     : Symbol_List_Map;
      --  map containing all children symbol lists

   end record;
   --  Type used for gpr file outline.

   package GPS renames Gpr_Parser_Support;
   package TDH renames GPS.Token_Data_Handlers;
   package PRA renames GPR2.Project.Registry.Attribute;

   type Type_Id is new Natural with Default_Value => 0;
   --  Used to describe variable's type's names

   No_Type : constant Type_Id := 0;
   --  Used by untyped variable

   function "+" (Name : String) return Type_Id;
   function Image (Id : Type_Id) return VSS.Strings.Virtual_String;
   --  Type_Id/String conversions

   type Variable_Id is new Natural with Default_Value => 0;
   --  Used to describe variable's names

   No_Variable : constant Variable_Id := 0;
   --  Used when there is no variable attached

   function "+" (Name : String) return Variable_Id;
   function Image (Id : Variable_Id) return VSS.Strings.Virtual_String;
   --  Variable_Id/String conversions

   type Project_Id is new Natural with Default_Value => 0;
   --  Used to describe project's names

   No_Project : constant Project_Id := 0;
   --  Used to describe current project in reference ex: project'Name

   function "+" (Name : String) return Project_Id;
   function Image (Id : Project_Id) return VSS.Strings.Virtual_String;
   --  Project_Id/String conversions

   use type TDH.Token_Index;

   type Index_Type is record
      Text      : VSS.Strings.Virtual_String;
      Compare   : VSS.Strings.Virtual_String;
      At_Pos    : Unit_Index := No_Index;
      Is_Others : Boolean := False;
   end record;
   --  Used to handle attribute's indexes.
   --  'Compare' field is used to compare indexes & generates hash key.

   use type VSS.Strings.Virtual_String;

   overriding function "=" (Left, Right : Index_Type) return Boolean is
     (Left.Compare = Right.Compare);

   function Hash (Index : Index_Type) return Ada.Containers.Hash_Type is
     (VSS.Strings.Hash (Index.Compare));

   function Is_Case_Sensitive
     (Index_Value  : Unbounded_String;
      Q_Attribute : GPR2.Q_Attribute_Id) return Boolean is
     (if PRA.Exists (Q_Attribute)
      then PRA.Is_Case_Sensitive (To_String (Index_Value),
        PRA.Get (Q_Attribute).Index_Type)
      else True);
   --  return if attribute index is case sensitive.

   function Create
     (Text        : Unbounded_String;
      Q_Attribute : GPR2.Q_Attribute_Id;
      At_Pos      : Unit_Index := GPR2.No_Index
     ) return Index_Type;
   --  Index_Type creator. 'others' index cannot be created.
   --  Index without 'at' value should use Unit_Index = GPR2.No_Index

   No_Index : constant Index_Type :=
                (Text      => VSS.Strings.Empty_Virtual_String,
                 Compare   => VSS.Strings.Empty_Virtual_String,
                 At_Pos    => GPR2.No_Index,
                 Is_Others => False);
   --  To be used for attribute without index (for Main use)

   Others_Index : constant Index_Type :=
     ("others",
      "others",
      GPR2.No_Index,
      True);
   --  'others' Index type value. ex: for Switches (others) use ("-g");

   type Attribute_Definition is record
      Index : Index_Type;       -- attribute's index.
      Token : Gpr_Parser.Common.Token_Reference;  -- token returned by gpr lexer
   end record;
   --  Used to add an attribute definition.

   package Attribute_Index_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Index_Type,
      Element_Type    => Attribute_Definition,
      Hash            => Hash,
      Equivalent_Keys => "=");
   --  Map containing attributes definitions for an attribute name
   --  One Attribute_Definition for each index defined.

   use type Attribute_Index_Maps.Map;

   package Attribute_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type        => Optional_Attribute_Id,
      Element_Type    => Attribute_Index_Maps.Map);
   --  Map containing all attributes in a project or a package.

   package Value_Type_List is
     new Ada.Containers.Vectors (Positive, TDH.Token_Index);
   --  List containing all string tokens defining possible values

   subtype Value_List is Value_Type_List.Vector;

   type Variable_Type is record
      Name    : Type_Id;
      --  Type's name

      Token   : Gpr_Parser.Common.Token_Reference;
      --  Token between 'type' and ':'

      Values  : Value_List;
      --  List of possible values
   end record;
   --  Used to store a type declaration

   package Type_Maps is
     new Ada.Containers.Ordered_Maps (Type_Id, Variable_Type);
   --  Map used to store all variable types of a project

   Untyped_Variable : constant Variable_Type :=
     (No_Type,
      Gpr_Parser.Common.No_Token,
      Value_Type_List.Empty_Vector);
   --  'Variable_Type' used when defining an untyped variable eg: Var := "";

   type Type_Reference is record
      Project : Project_Id;
      Name    : Type_Id;
   end record;
   --  Used by variable definition to provide type qualifier name.
   --  ex: Prj.Typ or Prj.Child.Typ

   type Variable_Definition is record
      Name     : Variable_Id;
      Token    : Gpr_Parser.Common.Token_Reference;
      Var_Type : Type_Reference;
   end record;
   --  Used to store a project's variable definition.

   package Variable_Maps is
     new Ada.Containers.Ordered_Maps (Variable_Id, Variable_Definition);
   --  Map for all variables defined in a project.
   --  Only the first definition of a variable is stored.
   --  Var := "first"; Var := "second";

   type Package_Definition is record
      Name               : Package_Id := GPR2.Project_Level_Scope;

      First              : Gpr_Parser.Common.Token_Reference :=
                             Gpr_Parser.Common.No_Token;
      --  'First' is at 'package' token

      Last               : Gpr_Parser.Common.Token_Reference :=
                             Gpr_Parser.Common.No_Token;
      --  'Last' is at the package's end or at the end of the renaming.

      Package_Range    : LSP.Structures.A_Range := ((0, 0), (0, 0));
      --  Useful to find to what package a LSP Position belongs.

      Attributes         : Attribute_Maps.Map;
      --  Attributes defined in the package

      Variables          : Variable_Maps.Map;
      --  Variables defined in the packages

      Referenced_Project : Project_Id := No_Project;
      --  project containing the renamed or extended package.

      Referenced_Package : Package_Id := GPR2.Project_Level_Scope;
      --  package that is renamed or extended.

      Renaming           : Boolean := False;
      --  True if package renames another

      Extending          : Boolean := False;
      --  True if package extends another

      --  TODO replace Renaming/Extending by Kind: RENAMING,EXTENDING,ROOT
   end record;
   --  A project's package definition
   --  'First' is at 'package' token
   --  'Last' is at the package's end statement or at the end of the renaming.

   Empty_Package_Definition : constant Package_Definition := (others => <>);

   package Package_Maps is
     new Ada.Containers.Ordered_Maps (Package_Id, Package_Definition);
   --  Map for all project's packages.

   use type GPR2.Path_Name.Object;

   package Token_To_File_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Gpr_Parser.Common.Token_Reference,
      Element_Type => GPR2.Path_Name.Object);
   --  Map for referenced projects navigation. (Key is Token_Index)

   package Name_To_File_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Project_Id,
      Element_Type => GPR2.Path_Name.Object);
   --  Map for referenced projects navigation. (Key is identifier)

   package Project_Id_List is
     new Ada.Containers.Vectors (Positive, Project_Id);
   --  Project_Id list used for imported & limited_imported projects

   type File (Tracer : not null LSP.Tracers.Tracer_Access) is tagged limited
      record
         Path : GPR2.Path_Name.Object;
         --  project path of this gpr file

         Search_Paths : GPR2.Path_Name.Set.Object :=
                          GPR2.Project.Default_Search_Paths
                            (False, GPR2.Environment.Process_Environment);

         Tab_Stop : Positive := Gpr_Parser_Support.Slocs.Default_Tab_Stop;
         --  tab expansion

         File_Provider : File_Provider_Access;
         --  provider used to get referenced gpr files

         Import_Partition_End : TDH.Token_Index := TDH.No_Token_Index;
         --  First token not in import partition.

         Project_Definition_Start : TDH.Token_Index := TDH.No_Token_Index;
         --  project definition first token (project, abstract, library, ... )

         Name : Project_Id := No_Project;
         --  project_id of this gpr file

         Name_Token : Gpr_Parser.Common.Token_Reference :=
                        Gpr_Parser.Common.No_Token;
         --  token containing the project name string

         Kind                     : GPR2.Project_Kind := GPR2.K_Standard;
         --  project kind

         Parsed                   : Boolean := False;
         --  True if file already parsed. Cleaned on file reset.

         Unit                     : Gpr_Parser.Analysis.Analysis_Unit;

         Token_To_File_Map        : Token_To_File_Maps.Map;
         --  token (imported/extended) to project name link

         Name_To_File_Map         : Name_To_File_Maps.Map;
         --  gpr file to project name link

         Imported                 : Project_Id_List.Vector;
         --  contains non limited imported files

         Limited_Imported         : Project_Id_List.Vector;
         --  contains all imported file (limited and non limited

         Extended                 : Project_Id := No_Project;
         --  extended gpr file project_id

         Extended_Path            : Path_Name.Object;
         --  extended gpr file path

         Extended_All             : Boolean := False;
         --  extending all a gpr

         Types                    : Type_Maps.Map;
         --  gpr file types

         Project_Level_Scope_Defs : Package_Definition;
         --  gpr file project level attributes/variables

         Packages                 : Package_Maps.Map;
         --  gpr file packages

         Document_Symbols         : GPR_Symbols;
         --  Used for gpr file outline. Document_Symbols in LSP language
      end record;

   -------------------
   -- String tables --
   -------------------

   package Name_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Natural,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   package Name_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String,
      "="          => Ada.Strings.Equal_Case_Insensitive);

   type Name_List is record
      Name_To_Id : Name_Maps.Map;
      Id_To_Name : VSS.String_Vectors.Virtual_String_Vector;
   end record;

   function Id
     (List : in out Name_List; Name : Optional_Name_Type) return Natural;
   function Image
     (List : Name_List; Id : Natural) return VSS.Strings.Virtual_String;

   Type_List : Name_List;
   --  registered type names

   function "+" (Name : String) return Type_Id is
     (Type_Id (Id (Type_List, GPR2.Optional_Name_Type (Name))));
   function Image (Id : Type_Id) return  VSS.Strings.Virtual_String is
     (Image (Type_List, Natural (Id)));

   Variable_List : Name_List;
   --  registered variable names

   function "+" (Name : String) return Variable_Id is
     (Variable_Id (Id (Variable_List, GPR2.Optional_Name_Type (Name))));
   function Image (Id : Variable_Id) return  VSS.Strings.Virtual_String is
     (Image (Variable_List, Natural (Id)));

   Project_List : Name_List;
   --  registered project names

   function "+" (Name : String) return Project_Id is
     (Project_Id (Id (Project_List, GPR2.Optional_Name_Type (Name))));
   function Image (Id : Project_Id) return  VSS.Strings.Virtual_String is
     (Image (Project_List, Natural (Id)));

   function In_Packages
     (Self  : File;
      Name : Package_Id) return Boolean is
      (Self.Packages.Contains (Name));

   function Kind (Self : File) return GPR2.Project_Kind is (Self.Kind);

   function To_String (Ref : Gpr_Parser.Common.Token_Reference) return String
   is (Gpr_Parser_Support.Text.To_UTF8 (Gpr_Parser.Common.Text (Ref)));
   --  convert a token reference to an utf8 string

   function To_Text_Type
     (Ref : Gpr_Parser.Common.Token_Reference)
      return Gpr_Parser_Support.Text.Text_Type
   is (Gpr_Parser.Common.Text (Ref));
   --  convert a token reference to a wide_wide_string

   function To_Lower_Text_Type
     (Ref : Gpr_Parser.Common.Token_Reference) return Gpr_Parser_Support.Text.Text_Type
   is (Gpr_Parser_Support.Text.To_Lower (To_Text_Type (Ref)));
   --  convert a token reference to a lowered wide_wide_string

   function To_Lower_String (Ref : Gpr_Parser.Common.Token_Reference) return String
   is (Gpr_Parser_Support.Text.To_UTF8 (To_Lower_Text_Type (Ref)));
   --  convert a token reference to a lowered string

   function Get_Line
     (Self : File; Line_Number : Integer)
      return VSS.Strings.Virtual_String is
     (VSS.Strings.To_Virtual_String
        (Self.Unit.Get_Line (Line_Number) &
           Ada.Characters.Wide_Wide_Latin_1.LF));

   function Token
     (Self     : File;
      Location : Gpr_Parser.Slocs.Source_Location)
      return Gpr_Parser.Common.Token_Reference is
      (Self.Unit.Lookup_Token (Location));

end LSP.GPR_Files;
