------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2024-2025, AdaCore                     --
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

with Ada.Containers.Generic_Array_Sort;
with GPR2;
with Gpr_Parser.Common;
with Gpr_Parser_Support.Slocs;
with Gpr_Parser_Support.Text;
with LSP.GPR_Files.References;
with VSS.Strings;

package body LSP.GPR_Highlighters is

   use all type Gpr_Parser.Common.Token_Kind;

   Type_Image : constant array (Token_Type) of VSS.Strings.Virtual_String :=
     (Namespace => "namespace",
      Enum      => "enum",
      Variable  => "variable",
      Property  => "property");

   Mod_Image : constant array (Token_Modifier) of VSS.Strings.Virtual_String :=
     (Declaration  => "declaration",
      Modification => "modification");

   procedure Append_Identifiers
     (Self   : GPR_Highlighter'Class;
      File   : LSP.GPR_Files.File_Access;
      Result : in out LSP.Structures.Natural_Vector;
      Prev   : in out Gpr_Parser_Support.Slocs.Source_Location;
      From   : Gpr_Parser.Common.Token_Reference;
      To     : Gpr_Parser.Common.Token_Reference;
      Pkg_Id : GPR2.Package_Id);
   --  Find all identifiers in range From..To from File and populate Result.
   --  Use (then update) Prev token to calculate relative line/column offsets.
   --  Package identifier Pkg_Id is used to calculate correct id reference.

   procedure Append
     (Self   : GPR_Highlighter'Class;
      Prev   : in out Gpr_Parser_Support.Slocs.Source_Location;
      Token  : Gpr_Parser.Common.Token_Reference;
      Kind   : Token_Type;
      Is_Def : Boolean;
      Result : in out LSP.Structures.Natural_Vector);
   --  Append a single Token of given Kind to Result.
   --  Use (then update) Prev token to calculate relative line/column offsets.
   --  Is_Def = True when the identifier is "the defining name".

   function Is_Identifier
     (Token : Gpr_Parser.Common.Token_Reference) return Boolean is
       (Gpr_Parser.Common.Data (Token).Kind = Gpr_Identifier);

   ------------
   -- Append --
   ------------

   procedure Append
     (Self   : GPR_Highlighter'Class;
      Prev   : in out Gpr_Parser_Support.Slocs.Source_Location;
      Token  : Gpr_Parser.Common.Token_Reference;
      Kind   : Token_Type;
      Is_Def : Boolean;
      Result : in out LSP.Structures.Natural_Vector)
   is
      use type Gpr_Parser_Support.Slocs.Line_Number;
      use type Gpr_Parser_Support.Slocs.Column_Number;

      subtype uint is Natural;

      function Token_Text return Gpr_Parser_Support.Text.Text_Type is
        (Gpr_Parser_Support.Text.To_Lower (Gpr_Parser.Common.Text (Token)));

      Sloc : constant Gpr_Parser_Support.Slocs.Source_Location_Range :=
        Gpr_Parser.Common.Sloc_Range
          (Gpr_Parser.Common.Data (Token));

      Start : constant Gpr_Parser_Support.Slocs.Source_Location :=
        Gpr_Parser_Support.Slocs.Start_Sloc (Sloc);

      Modif : constant Natural :=
        (if Is_Def and then Self.Has_Modifier (Declaration)
         then Self.Modifier_Id (Declaration) else 0);
   begin
      if Result.Is_Empty and then Token_Text = "project" then
         --  Skip the first "project" identifier used as the keyword
         null;
      elsif Self.Has_Type (Kind) then
         --  deltaLine
         Result.Append (uint (Start.Line - Prev.Line));
         --  deltaStartChar
         Result.Append
           (uint (Start.Column
                   - (if Start.Line = Prev.Line then Prev.Column else 1)));
         --  length
         Result.Append (uint (Sloc.End_Column - Sloc.Start_Column));
         --  tokenType
         Result.Append (Self.Type_Id (Kind));
         --  tokenModifiers
         Result.Append (Modif);

         Prev := Start;
      end if;
   end Append;

   ------------------------
   -- Append_Identifiers --
   ------------------------

   procedure Append_Identifiers
     (Self   : GPR_Highlighter'Class;
      File   : LSP.GPR_Files.File_Access;
      Result : in out LSP.Structures.Natural_Vector;
      Prev   : in out Gpr_Parser_Support.Slocs.Source_Location;
      From   : Gpr_Parser.Common.Token_Reference;
      To     : Gpr_Parser.Common.Token_Reference;
      Pkg_Id : GPR2.Package_Id)
   is
      use type Gpr_Parser.Common.Token_Reference;
      Token : Gpr_Parser.Common.Token_Reference := From;
      Ref   : LSP.GPR_Files.References.Reference;

      function Is_Def return Boolean is
        (LSP.GPR_Files.References.Token_Reference (Ref) = Token);

   begin
      while Token /= To loop
         if Is_Identifier (Token) then
            Ref := LSP.GPR_Files.References.Identifier_Reference
              (File, Pkg_Id, Token);

            case LSP.GPR_Files.References.Kind (Ref) is
               when LSP.GPR_Files.References.Project_Ref =>
                  Self.Append (Prev, Token, Namespace, Is_Def, Result);
               when LSP.GPR_Files.References.Type_Ref =>
                  Self.Append (Prev, Token, Enum, Is_Def, Result);

               when LSP.GPR_Files.References.Variable_Ref =>
                  Self.Append (Prev, Token, Variable, Is_Def, Result);

               when LSP.GPR_Files.References.Attribute_Ref =>
                  Self.Append (Prev, Token, Property, Is_Def, Result);

               when LSP.GPR_Files.References.Package_Ref =>
                  Self.Append (Prev, Token, Namespace, Is_Def, Result);

               when LSP.GPR_Files.References.No_Ref =>
                  null;
            end case;
         end if;

         Token := Gpr_Parser.Common.Next (Token);
      end loop;
   end Append_Identifiers;

   ----------------
   -- Get_Tokens --
   ----------------

   procedure Get_Tokens
     (Self   : GPR_Highlighter'Class;
      File   : LSP.GPR_Files.File_Access;
      Result : out LSP.Structures.Natural_Vector)
   is
      use type Gpr_Parser.Common.Token_Reference;

      function Before (L, R : LSP.GPR_Files.Package_Range) return Boolean is
        (L.First < R.First);

      procedure Sort is new Ada.Containers.Generic_Array_Sort
        (Positive,
         LSP.GPR_Files.Package_Range,
         LSP.GPR_Files.Package_Range_Array,
         Before);

      EOF  : constant Gpr_Parser.Common.Token_Reference :=
        Gpr_Parser.Common.No_Token;

      Packages : LSP.GPR_Files.Package_Range_Array := File.Package_Ranges;
      Next : Gpr_Parser.Common.Token_Reference := File.Token ((1, 1));
      Prev : Gpr_Parser_Support.Slocs.Source_Location := (1, 1);
   begin
      Sort (Packages);

      for Pkg of Packages loop
         Self.Append_Identifiers
           (File, Result, Prev, Next, Pkg.First, GPR2.Project_Level_Scope);

         Self.Append_Identifiers
           (File, Result, Prev, Pkg.First, Pkg.Last, Pkg.Package_Id);

         Next := Pkg.Last;
      end loop;

      Self.Append_Identifiers
        (File, Result, Prev, Next, EOF, GPR2.Project_Level_Scope);
   end Get_Tokens;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : in out GPR_Highlighter'Class;
      Types     : in out LSP.Structures.Virtual_String_Vector;
      Modifiers : in out LSP.Structures.Virtual_String_Vector)
   is
   begin
      declare
         Result : LSP.Structures.Virtual_String_Vector;
      begin
         for X in Token_Type loop
            if Types.Contains (Type_Image (X)) then
               Self.Type_Id (X) := Result.Length;
               Self.Has_Type (X) := True;
               Result.Append (Type_Image (X));
            end if;
         end loop;

         Types := Result;
      end;

      declare
         Result : LSP.Structures.Virtual_String_Vector;
      begin
         for X in Token_Modifier loop
            if Modifiers.Contains (Mod_Image (X)) then
               Self.Modifier_Id (X) := 2 ** Result.Length;
               Self.Has_Modifier (X) := True;
               Result.Append (Mod_Image (X));
            end if;
         end loop;

         Modifiers := Result;
      end;
   end Initialize;

end LSP.GPR_Highlighters;
