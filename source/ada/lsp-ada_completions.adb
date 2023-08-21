------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

with Ada.Containers.Hashed_Sets;

with VSS.Strings.Hash;

with LSP.Ada_Documents;
with LSP.Utils;

package body LSP.Ada_Completions is
   pragma Warnings (Off);

   ------------------
   -- Is_End_Token --
   ------------------

   function Is_End_Token
     (Token : Libadalang.Common.Token_Reference)
      return Boolean
   is
      use Libadalang.Common;
      End_Token : constant Libadalang.Common.Token_Data_Type :=
        Libadalang.Common.Data (Token);

      Token_Kind : constant Libadalang.Common.Token_Kind :=
        Libadalang.Common.Kind (End_Token);
   begin
      return Token_Kind = Libadalang.Common.Ada_End;
   end Is_End_Token;

   ------------------------
   -- Is_Full_Sloc_Equal --
   ------------------------

   function Is_Full_Sloc_Equal
     (Left, Right : Libadalang.Analysis.Defining_Name) return Boolean
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Is_Full_Sloc_Equal unimplemented");
      return
        raise Program_Error with "Unimplemented function Is_Full_Sloc_Equal";
   end Is_Full_Sloc_Equal;

   -----------------------
   -- Skip_Dotted_Names --
   -----------------------

   function Skip_Dotted_Names
     (Node : Libadalang.Analysis.Ada_Node)
      return Libadalang.Analysis.Ada_Node
   is
      use Libadalang.Common;
      Parent : Libadalang.Analysis.Ada_Node := Node;
   begin
      while not Parent.Is_Null
        and then Parent.Kind = Libadalang.Common.Ada_Dotted_Name
      loop
         Parent := Parent.Parent;
      end loop;

      return Parent;
   end Skip_Dotted_Names;

   -----------------------
   -- Write_Completions --
   -----------------------

   procedure Write_Completions
     (Context                  : LSP.Ada_Contexts.Context;
      Document                 : LSP.Ada_Documents.Document;
      Sloc                     : Langkit_Support.Slocs.Source_Location;
      Node                     : Libadalang.Analysis.Ada_Node;
      Names                    : Completion_Maps.Map;
      Named_Notation_Threshold : Natural;
      Compute_Doc_And_Details  : Boolean;
      Result                   : in out LSP.Structures.CompletionItem_Vector)
   is
      package String_Sets is new Ada.Containers.Hashed_Sets
        (VSS.Strings.Virtual_String,
         VSS.Strings.Hash,
         VSS.Strings."=",
         VSS.Strings."=");

      Seen   : String_Sets.Set;
      --  Set of found visible names in canonical form
      Length : constant Natural := Natural (Names.Length);
   begin

      --  Write Result in two pases. Firstly append all visible names and
      --  populate Seen set. Then append invisible names not in Seen.

      for Visible in reverse Boolean loop  --  Phase: True then False
         for Cursor in Names.Iterate loop
            declare
               Append    : Boolean := False;
               Info      : constant Name_Information := Names (Cursor);
               Name      : constant Libadalang.Analysis.Defining_Name :=
                 Completion_Maps.Key (Cursor);
               Selector  : constant Libadalang.Analysis.Name :=
                 Name.P_Relative_Name;
               Label     : VSS.Strings.Virtual_String;
               Canonical : VSS.Strings.Virtual_String;
            begin
               if Visible and Info.Is_Visible then
                  Label := VSS.Strings.To_Virtual_String (Selector.Text);
                  Canonical := LSP.Utils.Canonicalize (Label);
                  Seen.Include (Canonical);
                  Append := True;
               elsif not Visible and not Info.Is_Visible then
                  --  Append invisible name on if no such visible name found
                  Label := VSS.Strings.To_Virtual_String (Selector.Text);
                  Canonical := LSP.Utils.Canonicalize (Label);
                  Append := not Seen.Contains (Canonical);
               end if;

               if Append then
                  Result.Append
                    (LSP.Ada_Documents.Compute_Completion_Item
                       (Document                 => Document,
                        Context                  => Context,
                        Sloc                     => Sloc,
                        Node                     => Node,
                        BD                       => Name.P_Basic_Decl,
                        Label                    => Label,
                        Use_Snippets             => Info.Use_Snippets,
                        Compute_Doc_And_Details  => Compute_Doc_And_Details,
                        Named_Notation_Threshold => Named_Notation_Threshold,
                        Is_Dot_Call              => Info.Is_Dot_Call,
                        Is_Visible               => Info.Is_Visible,
                        Pos                      => Info.Pos,
                        Weight                   => Info.Weight,
                        Completions_Count        => Length));
               end if;
            end;
         end loop;
      end loop;
   end Write_Completions;

   --------------------------
   -- Pretty_Print_Snippet --
   --------------------------

   procedure Pretty_Print_Snippet
     (Context :    LSP.Ada_Contexts.Context; Prefix : String; Offset : Natural;
      Span    : LSP.Structures.A_Range; Rule : Libadalang.Common.Grammar_Rule;
      Result  : in out LSP.Structures.CompletionItem)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Pretty_Print_Snippet unimplemented");
      raise Program_Error with "Unimplemented procedure Pretty_Print_Snippet";
   end Pretty_Print_Snippet;

   ---------------------------
   -- Generic_Write_Symbols --
   ---------------------------

   procedure Generic_Write_Symbols
     (Names  :        Completion_Maps.Map;
      Result : in out LSP.Structures.DocumentSymbol_Vector)
   is
   begin
      pragma Assert (not Has_Been_Canceled);
      pragma Compile_Time_Warning
        (Standard.True, "Generic_Write_Symbols unimplemented");
      raise Program_Error with "Unimplemented procedure Generic_Write_Symbols";
   end Generic_Write_Symbols;

end LSP.Ada_Completions;
