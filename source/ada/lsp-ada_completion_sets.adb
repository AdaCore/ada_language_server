------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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

with GNATCOLL.VFS;

with LSP.Ada_Contexts;
with LSP.Ada_Documents;

package body LSP.Ada_Completion_Sets is

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
      Result                   : in out LSP.Messages.CompletionItem_Vector) is
   begin
      for Cursor in Names.Iterate loop
         declare
            Name : constant Libadalang.Analysis.Defining_Name :=
              Completion_Maps.Key (Cursor);
            Info : constant Name_Information := Names (Cursor);
         begin
            Result.Append
              (LSP.Ada_Documents.Compute_Completion_Item
                 (Context                  => Context,
                  BD                       => Name.P_Basic_Decl,
                  DN                       => Name,
                  Use_Snippets             => Info.Use_Snippets,
                  Named_Notation_Threshold => Named_Notation_Threshold,
                  Is_Dot_Call              => Info.Is_Dot_Call,
                  Is_Visible               => Info.Is_Visible));
         end;
      end loop;
   end Write_Completions;

end LSP.Ada_Completion_Sets;
