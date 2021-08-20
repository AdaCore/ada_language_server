------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

package body LSP.Ada_Completions.Filters is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Filter;
      Token : Libadalang.Common.Token_Reference;
      Node  : Libadalang.Analysis.Ada_Node)
   is
   begin
      Self.Token := Token;
      Self.Node := Node;
   end Initialize;

   ------------------
   -- Is_End_Label --
   ------------------

   function Is_End_Label (Self : in out Filter'Class) return Boolean is
   begin
      if not Self.Is_End_Label.Is_Set then
         declare
            use all type Libadalang.Common.Ada_Node_Kind_Type;
            use all type Libadalang.Common.Token_Kind;

            Prev_Token : constant Libadalang.Common.Token_Reference :=
              Libadalang.Common.Previous (Self.Token, Exclude_Trivia => True);

            Prev_Token_Kind : constant Libadalang.Common.Token_Kind :=
              Libadalang.Common.Kind (Libadalang.Common.Data (Prev_Token));

            Parent : Libadalang.Analysis.Ada_Node :=
              (if Self.Node.Is_Null then Self.Node else Self.Node.Parent);
         begin
            --  Get the outermost dotted name of which node is a prefix, so
            --  that when completing in a situation such as the following:
            --
            --      end Ada.Tex|
            --                 ^ Cursor here
            --
            --  we get the DottedName node rather than just the "Tex" BaseId.
            --  We want the DottedName rather than the Id so as to get the
            --  proper completions (all elements in the "Ada" namespace).

            while not Parent.Is_Null
              and then Parent.Kind = Ada_Dotted_Name
            loop
               Parent := Parent.Parent;
            end loop;

            Self.Is_End_Label :=
              (True,
               Prev_Token_Kind = Ada_End or else
                 (not Parent.Is_Null and then Parent.Kind = Ada_End_Name));
         end;
      end if;

      return Self.Is_End_Label.Value;
   end Is_End_Label;

end LSP.Ada_Completions.Filters;
