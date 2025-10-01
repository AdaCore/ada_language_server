------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2025, AdaCore                       --
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

package body LSP.Ada_Job_Contexts is

   ----------------------------
   -- Imprecise_Resolve_Name --
   ----------------------------

   procedure Imprecise_Resolve_Name
     (Self     : in out Ada_Job_Context'Class;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.TextDocumentPositionParams'Class;
      Name     : out Libadalang.Analysis.Defining_Name;
      Origin   : out Libadalang.Analysis.Ada_Node) is
   begin
      Origin := Self.Get_Node_At (Context, Position);
      Name   :=
        Self.Imprecise_Resolve_Name
          (Laltools.Common.Get_Node_As_Name (Origin));
   end Imprecise_Resolve_Name;

end LSP.Ada_Job_Contexts;
