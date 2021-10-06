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

with LSP.Types;

package LSP.Ada_Completions.Filters is

   type Filter is tagged limited private;
   --  The completion filter lets a completion provider skip well known
   --  completion context such as "end name", "numeric literal", etc

   procedure Initialize
     (Self   : in out Filter;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node);
   --  Initialize a completion filter with the completion position.
   --  For Token and Node description see Ada_Completions.Propose_Completion.

   function Is_End_Label (Self : in out Filter'Class) return Boolean;
   --  Check if we complete "end <name>" text

   function Is_Numeric_Literal (Self : in out Filter'Class) return Boolean;
   --  Check if we complete a numeric literal (even incomplete one, like 1E).

   function Is_Attribute_Ref (Self : in out Filter'Class) return Boolean;
   --  Check if we complete "'<attribute>" text

private

   type Filter is tagged limited record
      Token              : Libadalang.Common.Token_Reference;
      Node               : Libadalang.Analysis.Ada_Node;
      Is_End_Label       : LSP.Types.Optional_Boolean;
      Is_Numeric_Literal : LSP.Types.Optional_Boolean;
      Is_Attribute       : LSP.Types.Optional_Boolean;
   end record;

end LSP.Ada_Completions.Filters;
