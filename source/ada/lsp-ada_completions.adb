------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with VSS.Strings;

with LSP.Ada_Contexts;
with LSP.Ada_Documents;
with LSP.Lal_Utils;

package body LSP.Ada_Completions is

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal (Left, Right : Libadalang.Analysis.Defining_Name)
     return Boolean is
   begin
      return Libadalang.Analysis."=" (Left, Right);
   end Is_Equal;

   -----------------------
   -- Write_Completions --
   -----------------------

   procedure Write_Completions
     (Context                  : LSP.Ada_Contexts.Context;
      Names                    : Completion_Maps.Map;
      Named_Notation_Threshold : Natural;
      Result                   : in out LSP.Messages.CompletionItem_Vector)
   is
      function Hash (Value : VSS.Strings.Virtual_String)
        return Ada.Containers.Hash_Type
          is (Ada.Containers.Hash_Type'Mod (Value.Hash));

      package String_Sets is new Ada.Containers.Hashed_Sets
        (VSS.Strings.Virtual_String, Hash, VSS.Strings."=", VSS.Strings."=");

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
               Selector  : constant Libadalang.Analysis.Single_Tok_Node :=
                 Name.P_Relative_Name;
               Label     : VSS.Strings.Virtual_String;
               Canonical : VSS.Strings.Virtual_String;
            begin
               if Visible and Info.Is_Visible then
                  Label := LSP.Lal_Utils.To_Virtual_String (Selector.Text);
                  Canonical := LSP.Lal_Utils.Canonicalize (Label);
                  Seen.Include (Canonical);
                  Append := True;
               elsif not Visible and not Info.Is_Visible then
                  --  Append invisible name on if no such visible name found
                  Label := LSP.Lal_Utils.To_Virtual_String (Selector.Text);
                  Canonical := LSP.Lal_Utils.Canonicalize (Label);
                  Append := not Seen.Contains (Canonical);
               end if;

               if Append then
                  Result.Append
                    (LSP.Ada_Documents.Compute_Completion_Item
                       (Context                  => Context,
                        BD                       => Name.P_Basic_Decl,
                        Label                    => Label,
                        Use_Snippets             => Info.Use_Snippets,
                        Named_Notation_Threshold => Named_Notation_Threshold,
                        Is_Dot_Call              => Info.Is_Dot_Call,
                        Is_Visible               => Info.Is_Visible,
                        Pos                      => Info.Pos,
                        Completions_Count        => Length));
               end if;
            end;
         end loop;
      end loop;
   end Write_Completions;

   ---------------------------
   -- Generic_Write_Symbols --
   ---------------------------

   procedure Generic_Write_Symbols
     (Names  : Completion_Maps.Map;
      Result : in out LSP.Messages.Symbol_Vector) is
   begin
      for Cursor in Names.Iterate loop
         declare
            Name : constant Libadalang.Analysis.Defining_Name :=
              Completion_Maps.Key (Cursor);
            Node : Libadalang.Analysis.Ada_Node := Name.As_Ada_Node;
         begin
            while not Node.Is_Null and then
              Node.Kind not in Libadalang.Common.Ada_Basic_Decl
            loop
               Node := Node.Parent;
            end loop;

            if not Node.Is_Null then
               Result.Vector.Append
                 (LSP.Messages.SymbolInformation'
                    (name     => LSP.Lal_Utils.To_Virtual_String (Name.Text),
                     kind     => LSP.Lal_Utils.Get_Decl_Kind
                                  (Node.As_Basic_Decl),
                     location => LSP.Lal_Utils.Get_Node_Location
                                  (Name.As_Ada_Node),
                     alsIsAdaProcedure => <>,
                     tags              => LSP.Messages.Empty,
                     deprecated        => <>,
                     containerName => <>));
            end if;

            exit when Has_Been_Canceled;
         end;
      end loop;
   end Generic_Write_Symbols;

end LSP.Ada_Completions;
